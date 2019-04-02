module Engine.Graphics.Scroller
    ( module Engine.Graphics.Scroller.Types

    , newScroller
    , updateScroller
    , makeRenderScroller
    ) where

import Delude
import Linear
import Foreign (nullPtr)
import Graphics.GL
import Diagrams.TwoD.Transform (translate, scaleX, scaleY)

import Engine.Types (Engine, graphics)
import Engine.Context (getFramebufferSize)
import Engine.Common.Types
import Engine.Graphics.Types
import Engine.Graphics.Scroller.Types

import Engine.Graphics.Utils
import Engine.Graphics
import Engine.Graphics.TextureAtlas (assignCustomPage, swapCustomPage)

newScroller :: ScrollerConfig -> Engine us Scroller
newScroller conf = do
    (bw, bh) <- calcScrollerBufferSize $ conf^.bufferMargin
    sb <- createTextureBuffer bw bh nullPtr
    tb <- createTextureBuffer bw bh nullPtr
    atlas <- use $ graphics.textureAtlas
    pid <- assignCustomPage atlas sb
    fmap Scroller . newIORef $ ScrollerState
        { field_bufferMargin      = conf^.bufferMargin
        , field_sourceBuffer      = sb
        , field_targetBuffer      = tb
        , field_atlasCustomPageId = pid
        , field_drawScale         = 0
        , field_position          = 0
        , field_size  = Size (fromIntegral bw) (fromIntegral bh)
        , field_valid = False
        }

updateScroller
    :: Scroller
    -> Float
    -> V2 Float
    -> Size Float
    -> (BBox Float -> Engine us RenderAction)
    -> Engine us ()
updateScroller sr@(Scroller ref) viewScale viewPos viewSize renderInBBox = do
    scrollerBufferResize sr
    s <- readIORef ref
    let scrPos  = s^.position
    let scrSize = s^.size
    let scrBBox = mkBBoxCenter (viewScale *^ scrPos) scrSize
    let vieBBox = mkBBoxCenter (viewScale *^ viewPos) viewSize
    let isInvalid = not (s^.valid) || s^.drawScale /= viewScale
    let isPartial = not (bboxInsideBBox vieBBox scrBBox) && scrPos /= viewPos
    if | isInvalid -> updateFull s
       | isPartial -> updatePart s
       | otherwise -> return ()
    where
    updatePart s = updateFull s -- TODO: write partial updates
    updateFull s = do
        let src = s^.sourceBuffer
        let trg = s^.targetBuffer
        let pid = s^.atlasCustomPageId
        let scrollerSize = s^.size
        let viewM = positionToViewMatrix viewPos
        let projM = orthoProjectionFor scrollerSize $ def
                  & scale .~ realToFrac viewScale
        let viewProjM = projM !*! viewM
        -- let bbox = mkBBoxCenter viewPos scrollerSize
        let bbox = mkBBoxCenter viewPos (scrollerSize ^/ viewScale)
        withTextureBuffer trg $ do
            glClearColor 1 1 1 1
            glClear GL_COLOR_BUFFER_BIT
            draw viewProjM =<< renderInBBox bbox
        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid trg
        writeIORef ref $ s
            & sourceBuffer .~ trg
            & targetBuffer .~ src
            & drawScale    .~ viewScale
            & position     .~ viewPos
            & valid        .~ True

makeRenderScroller :: MonadIO m => Scroller -> m RenderAction
makeRenderScroller (Scroller ref) = do
    s <- readIORef ref
    let sw = realToFrac $ s^.size.width
    let sh = negate $ realToFrac $ s^.size.height
    return $ renderFromAtlas $ def
        & colorMix   .~ 0
        & customPage .~ Just (s^.atlasCustomPageId, unitRect)
        & scaleX (sw)
        & scaleY (sh)
        & translate (realToFrac <$> (s^.position ^* s^.drawScale))

--------------------------------------------------------------------------------

scrollerBufferResize :: Scroller -> Engine us ()
scrollerBufferResize (Scroller ref) = do
    s <- readIORef ref
    (bw, bh) <- calcScrollerBufferSize $ s^.bufferMargin
    if s^.sourceBuffer.width == bw && s^.sourceBuffer.height == bh
    then return ()
    else do
        newSrc <- createTextureBuffer bw bh nullPtr
        newTrg <- createTextureBuffer bw bh nullPtr
        doneTextureBuffer $ s^.sourceBuffer
        doneTextureBuffer $ s^.targetBuffer
        writeIORef ref $ s
            & size         .~ Size (fromIntegral bw) (fromIntegral bh)
            & sourceBuffer .~ newSrc
            & targetBuffer .~ newTrg
            & valid        .~ False

calcScrollerBufferSize :: ScrollerMargin -> Engine us (GLint, GLint)
calcScrollerBufferSize m = do
    (w, h) <- getFramebufferSize =<< use (graphics.context)
    maxTexSize <- fromIntegral <$> glGetInteger GL_MAX_TEXTURE_SIZE
    let bw = mkBufferSize maxTexSize w (fromIntegral $ max w h)
    let bh = mkBufferSize maxTexSize h (fromIntegral $ max w h)
    return (bw, bh)
    where
    mkBufferSize :: Int -> Int -> Float -> GLint
    mkBufferSize maxSize s r = fromIntegral $ min maxSize $ case m of
        ScrollerMargin_Pixels  p -> p + s + p
        ScrollerMargin_Percent p -> let x = ceiling $ p * r in x + s + x

