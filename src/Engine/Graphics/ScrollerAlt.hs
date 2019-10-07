module Engine.Graphics.ScrollerAlt
    ( module Engine.Graphics.Scroller.TypesAlt

    , new
    , update
    , makeRenderAction

    ) where

import Delude
import Foreign (nullPtr)
import Linear
import Graphics.GL
import Engine.Graphics.Utils
import Engine.Types (Engine, graphics)
import Engine.Common.Types
import Engine.Context (getFramebufferSize)
import Engine.Graphics (RenderAction)
import Engine.Graphics.Scroller.TypesAlt
import Engine.Graphics.TextureAtlas
    (TextureAtlas, assignCustomPage, swapCustomPage)
import Engine.Graphics.Utils
import Engine.Graphics
import Diagrams.TwoD.Transform (translate, scaleX, scaleY)

--------------------------------------------------------------------------------

new :: ScrollerConfig -> Engine u Scroller
new conf = do
    siz <- getSize
    fb <- makeScrollerBuffer siz
    bb <- makeScrollerBuffer siz
    atlas <- use $ graphics.textureAtlas
    pid <- assignCustomPage atlas fb
    fmap Wrapped . newIORef $ ScrollerState
        { field_frontBuffer       = fb
        , field_backBuffer        = bb
        , field_valid             = False
        , field_position          = 0
        , field_config            = conf
        , field_atlasCustomPageId = pid
        }

type RenderCallback u = BBox Float -> Engine u ()
update
    :: Scroller
    -> V2 Float -- ^ Position
    -> RenderCallback u
    -> Engine u ()
update sr@(Scroller ref) pos renderInBBox = do
    scrollerResize sr
    s <- readIORef ref
    if s^.valid
    then if pos /= s^.position
        then updateShift s
        else return ()
    else updateFull s
    where
    updateShift s = do
        putStrLn "updateShift"
        let viewScale = s^.config.scale
        let off = floor <$> ((pos - s^.position) ^* viewScale)
        print off
        let src = s^.frontBuffer
        let trg = s^.backBuffer
        withTextureBuffer trg $ do
            glClearColor 0 0 0 1
            glClear GL_COLOR_BUFFER_BIT
        shiftTexture off src trg

        let siz = fromIntegral <$> s^.frontBuffer.size
        let p0  = (s^.position) ^* viewScale
        let p1  = pos           ^* viewScale
        let mvo = vertOverflow p0 p1 siz

        whenJust mvo $ \(reg, bb) -> do
            drawRegion reg trg $ do
                glClearColor 0 1 0 1
                glClear GL_COLOR_BUFFER_BIT
                renderInBBox bb

        let mho = horiOverflow p0 p1 siz
        whenJust mho $ \(reg, bb) -> do
            drawRegion reg trg $ do
                glClearColor 1 0 1 1
                glClear GL_COLOR_BUFFER_BIT
                renderInBBox bb

        swapPage s trg
        writeIORef ref $ s
            & valid        .~ True
            & position     .~ pos
            & frontBuffer  .~ trg
            & backBuffer   .~ src

    updateFull s = do
        putStrLn "updateFull"
        let fb = s^.frontBuffer
        let fullRegion = Rect 0 $ fb^.size

        drawRegion fullRegion fb $ do
            glClearColor 0 0 1 1
            glClear GL_COLOR_BUFFER_BIT
            renderInBBox $ BBox 0 0

        swapPage s fb
        writeIORef ref $ s
            & valid     .~ True
            & position  .~ pos

    drawRegion r buf = withTextureBuffer buf . withScissor r

    swapPage s buf = do
        let pid = s^.atlasCustomPageId
        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid buf

{-
    let fbSize = fromIntegral <$> buf^.size
    let viewScale = s^.config.scale

    let viewM = positionToViewMatrix pos
    let projM = orthoProjectionFor fbSize $ def
                & scale .~ realToFrac viewScale
    let viewProjM = projM !*! viewM
-}

vertOverflow :: V2 Float -> V2 Float -> Size Float -> Maybe (Rect Int32, BBox Float)
vertOverflow a b s
    | amY == bmY = Nothing
    | amY >= bxY || bmY >= axY = Nothing -- [(calcvp bb, bb)]
    | amY < bmY = Just (calcvp bbtop, bbtop)
    | amY > bmY = Just (calcvp bbbot, bbbot)
    | otherwise = Nothing
    where
    bbtop = BBox (V2 bmX axY) (V2 bxX bxY)
    bbbot = BBox (V2 bmX bmY) (V2 bxX amY)
    BBox (V2 amX amY) (V2 axX axY) = mkBBoxCenter a s
    BBox (V2 bmX bmY) (V2 bxX bxY) = bb
    bb = mkBBoxCenter b s

    v = bb^.minPoint
    -- calcvp :: BBox Float -> BBox Int
    -- calcvp (BBox mn mx) = bboxToRect $ fmap ceiling $ BBox (mn-v) (mx-v)
    calcvp (BBox mn mx) = bboxToRect $ BBox (floor <$> mn-v) (ceiling <$> mx-v)

horiOverflow :: V2 Float -> V2 Float -> Size Float -> Maybe (Rect Int32, BBox Float)
horiOverflow a b s
    | amX == bmX = Nothing
    | amX >= bxX || bmX >= axX = Nothing -- [(calcvp bb, bb)]
    | amX < bmX = Just (calcvp bblef, bblef)
    | amX > bmX = Just (calcvp bbrig, bbrig)
    | otherwise = Nothing
    where
    bblef = BBox (V2 axX bmY) (V2 bxX bxY)
    bbrig = BBox (V2 bmX bmY) (V2 amX bxY)
    BBox (V2 amX amY) (V2 axX axY) = mkBBoxCenter a s
    BBox (V2 bmX bmY) (V2 bxX bxY) = bb
    bb = mkBBoxCenter b s

    v = bb^.minPoint
    -- calcvp :: BBox Float -> BBox Int
    -- calcvp (BBox mn mx) = bboxToRect $ BBox (floor <$> mn-v) (ceiling <$> mx-v)
    calcvp (BBox mn mx) = bboxToRect $ BBox (floor <$> mn-v) (ceiling <$> mx-v)

withScissor :: MonadIO m => Rect Int32 -> m r -> m r
withScissor r action = do
    let V2   x y = r^.offset
    let Size w h = r^.size
    glScissor x y w h
    glEnable GL_SCISSOR_TEST
    ret <- action
    glDisable GL_SCISSOR_TEST
    return ret

makeRenderAction :: Scroller -> Engine u RenderAction
makeRenderAction sr = do
    s <- readIORef $ Unwrapped sr
    let sw = realToFrac $ s^.frontBuffer.width
    let sh = negate $ realToFrac $ s^.frontBuffer.height
    let pos = realToFrac <$> (s^.position ^* s^.config.scale)
    return $ renderFromAtlas $ def
        & colorMix   .~ 0
        & customPage .~ Just (s^.atlasCustomPageId, unitRect)
        & scaleX sw
        & scaleY sh
        & translate pos

--------------------------------------------------------------------------------

scrollerResize :: Scroller -> Engine us ()
scrollerResize (Scroller ref) = do
    siz@(Size bw bh) <- getSize
    s <- readIORef ref
    when (s^.frontBuffer.width /= bw || s^.frontBuffer.height /= bh) $ do
        putStrLn "scrollerResize"
        newFront <- makeScrollerBuffer siz
        newBack  <- makeScrollerBuffer siz
        doneTextureBuffer $ s^.frontBuffer
        doneTextureBuffer $ s^.backBuffer
        writeIORef ref $ s
            & frontBuffer .~ newFront
            & backBuffer  .~ newBack
            & valid       .~ False

getSize :: Engine u (Size Int32)
getSize = do
    (w, h) <- getFramebufferSize =<< use (graphics.context)
    maxTexSize <- glGetInteger GL_MAX_TEXTURE_SIZE
    let bw = min maxTexSize $ fromIntegral w
    let bh = min maxTexSize $ fromIntegral h
    return $ Size bw bh

makeScrollerBuffer :: Size Int32 -> Engine u TextureBuffer
makeScrollerBuffer s = createTextureBuffer (s^.width) (s^.height) nullPtr

