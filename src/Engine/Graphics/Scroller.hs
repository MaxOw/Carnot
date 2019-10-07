module Engine.Graphics.Scroller
    ( module Engine.Graphics.Scroller.Types

    , newScroller
    , updateScroller ,updateScroller'
    , makeRenderScroller

    , testCopyOffset
    , diffTest

    , calcScrollerBufferSize
    -- , calcRedrawRanges, bb0, bb1
    ) where

import Delude
import Linear
import Foreign (nullPtr)
import Graphics.GL
import Diagrams.TwoD.Transform (translate, scaleX, scaleY)

import qualified Control.Concurrent.Async as Async

import Engine.Types (Engine, graphics)
import Engine.Context (getFramebufferSize)
import Engine.Common.Types
import Engine.Graphics.Types
import Engine.Graphics.Scroller.Types

import Engine.Lens.Utils
import Engine.Graphics.Utils
import Engine.Graphics
import Engine.Graphics.TextureAtlas
    (TextureAtlas, assignCustomPage, swapCustomPage)

import Engine.Graphics.Scroller.Cells ()

--------------------------------------------------------------------------------

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
        , field_drawPosition      = 0
        , field_position          = 0
        , field_size       = Size (fromIntegral bw) (fromIntegral bh)
        , field_valid      = False
        , field_asyncBatch = Nothing
        }

updateScroller
    :: Scroller
    -> Float
    -> V2 Float
    -> Size Float
    -> (BBox Float -> Engine us RenderAction)
    -> Engine us ()
updateScroller sr = updateScroller' sr False
-- updateScroller sr = updateScrollerSync sr False

updateScroller'
    :: Scroller
    -> Bool
    -> Float
    -> V2 Float
    -> Size Float
    -> (BBox Float -> Engine us RenderAction)
    -> Engine us ()
updateScroller'
    sr@(Scroller ref) forceRedraw viewScale viewPos triggerSize renderInBBox
    = do
    scrollerBufferResize sr
    s <- readIORef ref
    case s^.ff#asyncBatch of
        Nothing -> updateMaybe s
        Just aBatch -> if forceRedraw
            then updateForce s
            else liftIO (Async.poll aBatch) >>= \case
                Nothing -> return ()
                Just eb -> case eb of
                    Left _e -> return () -- updateMaybe s
                    Right b -> drawUpdate s b
    where
    updateMaybe s = do
        let pos = s^.position
        let tbox = mkBBoxCenter pos triggerSize
        unless (bboxInside viewPos tbox && s^.valid) $ updateForce s
        -- if s^.valid
        -- then unless (bboxInside viewPos tbox) $ updateShift s
        -- else updateForce s

    updateShift s = do
        let off = floor <$> (viewPos - s^.position) ^* viewScale
        let src = s^.sourceBuffer
        let trg = s^.targetBuffer
        let pid = s^.atlasCustomPageId
        withTextureBuffer trg $ do
            glClearColor 1 1 1 1
            glClear GL_COLOR_BUFFER_BIT
        shiftTexture off src trg

        let ss = s^.size ^/ viewScale
        let dd = diffBBox viewPos (s^.position) ss
        let ma = Nothing
        {-
        ma <- case dd of
            [bb] -> do
                prep <- getPrepBatchIO
                ra <- renderInBBox bb
                ab <- liftIO $ Async.async $ do
                    evaluateNF =<< prep =<< batchRenderAction ra
                return $ Just ab
            _ -> return Nothing
        -}
        -- TODO: calculate overflow after shift
        -- schedule async computation to render overflow
        -- fix drawUpdate to draw on target without swapping
        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid trg
        writeIORef ref $ s
            & sourceBuffer  .~ trg
            & targetBuffer  .~ src
            & ff#drawPosition .~ viewPos
            & ff#asyncBatch   .~ ma

    updateForce s = do
        -- putStrLn "updateForce"
        cancelBatch s

        let scrSize = s^.size
        -- let bbox = mkBBoxCenter viewPos scrSize
        let bbox = mkBBoxCenter viewPos (scrSize ^/ viewScale)

        prep <- getPrepBatchIO
        ra <- renderInBBox bbox
        ab <- liftIO $ Async.async $ do
            evaluateNF =<< prep =<< batchRenderAction ra

        writeIORef ref $ s
            & valid           .~ True
            & drawScale       .~ viewScale
            & ff#drawPosition .~ viewPos
            & ff#asyncBatch   .~ Just ab

    drawUpdate s b = do
        -- putStrLn "drawUpdate"
        let src = s^.sourceBuffer
        let trg = s^.targetBuffer
        let pid = s^.atlasCustomPageId

        let scrSize      = s^.size
        let scrDrawPos   = s^.ff#drawPosition
        let scrViewScale = s^.drawScale

        let viewM = positionToViewMatrix scrDrawPos
        let projM = orthoProjectionFor scrSize $ def
                  & scale .~ realToFrac scrViewScale
        let viewProjM = projM !*! viewM

        withTextureBuffer trg $ do
            glClearColor 0 0 1 1
            glClear GL_COLOR_BUFFER_BIT
            drawBatch viewProjM b

        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid trg
        writeIORef ref $ s
            & sourceBuffer  .~ trg
            & targetBuffer  .~ src
            & position      .~ scrDrawPos
            & ff#asyncBatch .~ Nothing

updateScrollerSync
    :: Scroller
    -> Bool
    -> Float
    -> V2 Float
    -> Size Float
    -> (BBox Float -> Engine us RenderAction)
    -> Engine us ()
updateScrollerSync
    sr@(Scroller ref) forceRedraw viewScale viewPos triggerSize renderInBBox
    = do
    scrollerBufferResize sr
    s <- readIORef ref
    if forceRedraw || (not $ s^.valid)
    then updateForce s
    else updateMaybe s
    where
    updateMaybe s = do
        let pos = s^.position
        let tbox = mkBBoxCenter pos triggerSize
        unless (bboxInside viewPos tbox) $ updateShift s

    updateShift s = do
        putStrLn "updateShift"
        let off = floor <$> (viewPos - s^.position) ^* viewScale
        let src = s^.sourceBuffer
        let trg = s^.targetBuffer
        let pid = s^.atlasCustomPageId
        withTextureBuffer trg $ do
            glClearColor 0 0 0 1
            glClear GL_COLOR_BUFFER_BIT
        shiftTexture off src trg

        let ss = s^.size ^/ viewScale
        let dd = diffBBox viewPos (s^.position) ss
        {-
        ma <- case dd of
            [bb] -> do
                prep <- getPrepBatchIO
                ra <- renderInBBox bb
                ab <- liftIO $ Async.async $ do
                    evaluateNF =<< prep =<< batchRenderAction ra
                return $ Just ab
            _ -> return Nothing
        -}
        -- TODO: calculate overflow after shift
        -- schedule async computation to render overflow
        -- fix drawUpdate to draw on target without swapping
        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid trg
        writeIORef ref $ s
            & valid         .~ True
            & sourceBuffer  .~ trg
            & targetBuffer  .~ src
            & drawScale     .~ viewScale
            & position      .~ viewPos

        s' <- readIORef ref
        case dd of
            [(vp, bb)] -> do
                print bb
                print vp
                ra <- renderInBBox bb
                drawUpdate 1 (Just vp) s' ra
            _ -> return ()

    updateForce s = do
        putStrLn "updateForce"

        let scrSize = s^.size
        let bbox = mkBBoxCenter viewPos (scrSize ^/ viewScale)

        -- prep <- getPrepBatchIO
        ra <- renderInBBox bbox
        drawUpdate 0 Nothing s ra
        -- drawUpdate s =<< liftIO (prep =<< batchRenderAction ra)

    drawUpdate c mv s b = do
        -- putStrLn "drawUpdate"
        -- let trg = s^.sourceBuffer
        let trg = s^.sourceBuffer

        let scrSize      = s^.size
        let scrDrawPos   = viewPos -- s^.ff#drawPosition
        let scrViewScale = viewScale

        let viewM = positionToViewMatrix scrDrawPos
        let projM = orthoProjectionFor scrSize $ def
                  & scale .~ realToFrac scrViewScale
        let viewProjM = projM !*! viewM

        let f = case mv of
                Nothing -> withTextureBuffer trg
                Just vp -> withTextureBufferPart trg $ bboxToRect vp
        f $ do
        -- withTextureBufferPart trg (Rect 0 100) $ do -- $ bboxToRect vp
            {-
            whenJust mv $ \vp -> do
                let (V2 x y) = vp^.minPoint
                let (Size w h) = bboxSize vp
                -- glViewport x y w h
                glViewport 0 0 100 100
            -}
            glClearColor c 0 1 1
            glClear GL_COLOR_BUFFER_BIT
            -- drawBatch viewProjM b
            draw viewProjM b

        atlas <- use $ graphics.textureAtlas
        writeIORef ref $ s
            & valid     .~ True
            & drawScale .~ viewScale
            & position  .~ scrDrawPos

{-
diffBBox :: V2 Float -> V2 Float -> Size Float -> [BBox Float]
diffBBox a@(V2 xa ya) b@(V2 xb yb) s@(Size w h)
    | xa == xb =
        if ya == yb
        then []
        else [ybox]
    | otherwise =
        if ya == yb
        then []-- xbox]
        else [ybox] -- , xbox]
    where
    -- xbox = undefined
    ybox = mkBBoxCenter (V2 xb yy) (Size w yh)
    yh = abs (ya - yb)
    yy = yb + (yb - ya)
-}

diffBBox :: V2 Float -> V2 Float -> Size Float -> [(BBox Int32, BBox Float)]
diffBBox a b s
    | amY == bmY = []
    | amY >= bxY || bmY >= axY = [(calcvp bb, bb)]
    | amY < bmY = [(calcvp bbtop, bbtop)]
    | amY > bmY = [(calcvp bbbot, bbbot)]
    | otherwise = []
    where
    bbtop = BBox (V2 bmX axY) (V2 bxX bxY)
    bbbot = BBox (V2 bmX bmY) (V2 bxX amY)
    BBox (V2 amX amY) (V2 axX axY) = mkBBoxCenter a s
    bb = mkBBoxCenter b s
    BBox (V2 bmX bmY) (V2 bxX bxY) =  bb

    v = bb^.minPoint
    -- calcvp :: BBox Float -> BBox Int
    calcvp (BBox mn mx) = fmap floor $ BBox (mn-v) (mx-v)

diffTest :: IO ()
diffTest = do
    print $ diffBBox 0   0  2
    print $ diffBBox 0   1  2
    print $ diffBBox 0 (-1) 2
    print $ diffBBox 0   2  2
    print $ diffBBox 0 (-2) 2

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

cancelBatch :: ScrollerState -> Engine us ()
cancelBatch s = case s^.ff#asyncBatch of
    Nothing     -> return ()
    Just aBatch -> liftIO $ Async.cancel aBatch

--------------------------------------------------------------------------------

scrollerBufferResize :: Scroller -> Engine us ()
scrollerBufferResize (Scroller ref) = do
    s <- readIORef ref
    (bw, bh) <- calcScrollerBufferSize $ s^.bufferMargin
    if s^.sourceBuffer.width == bw && s^.sourceBuffer.height == bh
    then return ()
    else do
        putStrLn "scrollerBufferResize"
        newSrc <- createTextureBuffer bw bh nullPtr
        newTrg <- createTextureBuffer bw bh nullPtr
        doneTextureBuffer $ s^.sourceBuffer
        doneTextureBuffer $ s^.targetBuffer
        cancelBatch s
        writeIORef ref $ s
            & size          .~ Size (fromIntegral bw) (fromIntegral bh)
            & sourceBuffer  .~ newSrc
            & targetBuffer  .~ newTrg
            & valid         .~ False
            & ff#asyncBatch .~ Nothing

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

testCopyOffset :: MonadIO m => TextureAtlas -> m RenderAction
testCopyOffset atlas = do
    let (bw, bh) = (64, 64)
    source <- createTextureBuffer bw bh nullPtr
    target <- createTextureBuffer bw bh nullPtr
    pid <- assignCustomPage atlas target
    -- render to source
    withTextureBuffer source $ do
        glClearColor 1 0 0 1
        glClear GL_COLOR_BUFFER_BIT
    -- copy source to target with offset
    shiftTexture (V2 (-20) 10) source target
    -- render target to screen
    return $ renderFromAtlas $ def
        & colorMix   .~ 0
        & customPage .~ Just (pid, unitRect)
        & scaleX (fromIntegral bw)
        & scaleY (fromIntegral bh)

--------------------------------------------------------------------------------

{-
calcRedrawRanges :: BBox Float -> BBox Float -> [BBox Float]
calcRedrawRanges old new = hh <> vv
    where
    V2 ox oy = old^.minPoint
    V2 nx ny = new^.minPoint

    hh | oy < ny   = [new & minPoint._y .~ old^.maxPoint._y]
       | oy > ny   = [new & maxPoint._y .~ old^.minPoint._y]
       | otherwise = []

    g = new
      & minPoint._y .~ max (old^.minPoint._y) (new^.minPoint._y)
      & maxPoint._y .~ min (old^.maxPoint._y) (new^.maxPoint._y)

    vv | ox < nx   = [g & minPoint._x .~ old^.maxPoint._x]
       | ox > nx   = [g & maxPoint._x .~ old^.minPoint._x]
       | otherwise = []

bb0 :: BBox Float
bb0 = mkBBoxCenter (pure 0) (pure 2)

bb1 :: BBox Float
bb1 = mkBBoxCenter (pure 1) (pure 2)
-}

{-
    if forceRedraw
    then updateFull s
    else do
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
        let scrSize = s^.size
        let viewM = positionToViewMatrix viewPos
        let projM = orthoProjectionFor scrSize $ def
                  & scale .~ realToFrac viewScale
        let viewProjM = projM !*! viewM
        -- let bbox = mkBBoxCenter viewPos scrSize
        let bbox = mkBBoxCenter viewPos (scrSize ^/ viewScale)
        withTextureBuffer trg $ do
            glClearColor 1 1 1 1
            glClear GL_COLOR_BUFFER_BIT
            -- draw viewProjM =<< renderInBBox bbox
            renderInBBox bbox viewProjM
        atlas <- use $ graphics.textureAtlas
        swapCustomPage atlas pid trg
        writeIORef ref $ s
            & sourceBuffer .~ trg
            & targetBuffer .~ src
            & drawScale    .~ viewScale
            & position     .~ viewPos
            & valid        .~ True
-}

