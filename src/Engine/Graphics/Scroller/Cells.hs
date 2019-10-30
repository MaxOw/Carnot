module Engine.Graphics.Scroller.Cells
    ( module Engine.Graphics.Scroller.TypesCells

    , new
    , update
    , makeRenderAction

    ) where

import Delude
import qualified Data.Set as Set
import Foreign (nullPtr)
import Linear
import Graphics.GL
import Engine.Graphics.Utils
import Engine.Types (Engine, graphics)
import Engine.Common.Types
import Engine.Context (getFramebufferSize)
import Engine.Graphics (RenderAction)
import Engine.Graphics.TextureAtlas
    (TextureAtlas, assignCustomPage, swapCustomPage)
import Engine.Lens.Utils
import Engine.Graphics.Utils
import Engine.Graphics
import Diagrams.TwoD.Transform (translate, scaleX, scaleY)

import Engine.Graphics.Scroller.TypesCells
import Engine.Graphics.TaskManager (addTask)

--------------------------------------------------------------------------------

new :: ScrollerConfig -> Engine u Scroller
new conf = do
    maxTexSize <- min 8192 <$> glGetInteger GL_MAX_TEXTURE_SIZE
    -- putStrLn $ "GL_MAX_TEXTURE_SIZE: " <> show maxTexSize
    buf <- createTextureBuffer maxTexSize maxTexSize nullPtr
    let cs = conf^.ff#cellSize
    let cc = div maxTexSize (conf^.ff#cellSize)
    atlas <- use $ graphics.textureAtlas
    pid <- assignCustomPage atlas buf
    fmap Scroller . newIORef $ ScrollerState
        { field_buffer     = buf
        , field_freedCells = mkCells cs cc
        , field_validCells = []
        , field_position   = 0
        , field_config     = conf
        , field_atlasPid   = pid
        }

    where
    mkCells s c = mkCell <$> [0..c-1] <*> [0..c-1]
        where
        mkCell x y = ScrollerCell
            { field_cellPosition = 0
            , field_bufferRegion = s *^ V2 x y
            }

type RenderCallback u = BBox Float -> Engine u RenderAction
update
    :: Scroller
    -> Bool -- update full render region
    -> V2 Float -- ^ Position
    -> RenderCallback u
    -> Engine u ()
update sr@(Scroller ref) updateFullRR pos renderInBBox = do
    s <- readIORef ref
    let conf = s^.config
    rr <- getRenderRegion pos conf
    let err = extd rr
    let prioritySet = bboxCellsSet rr
    let allInRR = bboxCellsSet err
    let validInRR = Set.fromList $ map field_cellPosition
                  $ cellsInRange err $ s^.ff#validCells
    let missing = Set.difference allInRR validInRR
    let priority = Set.intersection prioritySet missing
    let fc = length $ s^.ff#freedCells
    let diff = Set.size missing - fc
    when (not $ Set.null missing) $ do
        when (diff > 0) $ gc sr pos diff
        if updateFullRR && not (Set.null priority)
        then forM_ (Set.toList priority) $ drawOneCell sr renderInBBox pos
        else do
            let ls = sortOn (distToCell conf pos) $ Set.toList missing
            whenNotNull ls $ drawOneCell sr renderInBBox pos . head

    modifyIORef' ref $ set position pos
    where

    bboxCellsSet (BBox (V2 mx my) (V2 xx xy))
        = Set.fromList $ V2 <$> [mx .. xx] <*> [my .. xy]
    extd (BBox m x) = BBox (pred <$> m) (succ <$> x)

cellsInRange :: BBox Int -> [ScrollerCell] -> [ScrollerCell]
cellsInRange rr = filter (flip bboxInside rr . view (ff#cellPosition))

getRenderRegion :: V2 Float -> ScrollerConfig -> Engine u (BBox Int)
getRenderRegion pos conf = do
    let s  = conf^.ff#scale
    let cs = conf^.ff#cellSize
    sz <- getFramebufferSize =<< use (graphics.context)
    let (w, h) = over each ((/2) . (/s) . fromIntegral) sz
    let toCell x = floor $ x*s / fromIntegral cs
    let gtop = toCell $ pos^._y + h
    let gbot = toCell $ pos^._y - h
    let glef = toCell $ pos^._x - w
    let grig = toCell $ pos^._x + w
    return $ BBox (V2 glef gbot) (V2 grig gtop)

drawOneCell
    :: Scroller
    -> RenderCallback u
    -> V2 Float -- ^ Position
    -> V2 Int   -- ^ Cell to draw
    -> Engine u ()
drawOneCell sr@(Scroller ref) renderInBBox pos cellToDraw = do
    s <- readIORef ref
    let conf = s^.config
    let fs = s^.ff#freedCells
    -- just draw one for now
    whenJust (viaNonEmpty head fs) $ \v -> do
        let m  = cellToDraw
        let vv = v & ff#cellPosition .~ m
        -- print m
        let buf       = s^.buffer
        let cs        = s^.config.ff#cellSize
        let ss        = fromIntegral cs / (s^.config.ff#scale)
        let reg       = Rect (v^.ff#bufferRegion) (pure cs)
        let spos      = negate <$> fromCellPos ss m
        let viewM     = mkMatHomo2 (translate spos mempty)
        let projM     = orthoProjectionFor (pure ss) def
        let projViewM = projM !*! viewM
        let bb        = rectToBBox $ ((*ss) . fromIntegral) <$> Rect m 1

        ra <- renderInBBox bb

        batchAsync <- getBatchAndPrepIO
        drawBatchOpts <- getDrawBatchOptsIO
        tm <- use $ graphics.taskManager
        addTask tm (batchAsync ra) $ \b ->
            withTextureBufferPart buf reg $ do
                glClearColor 1 1 1 1
                glClear GL_COLOR_BUFFER_BIT
                drawBatchOpts True projViewM b

        writeIORef ref $ s
            & ff#freedCells %~ drop 1
            & ff#validCells %~ (vv:)

fromCellPos :: Float -> V2 Int -> V2 Float
fromCellPos sc x = ((fromIntegral <$> x) + 0.5) ^* sc

distToCell :: ScrollerConfig -> V2 Float -> V2 Int -> Float
distToCell conf pos c = distance pos cpos
    where
    cpos = fromCellPos (cs/ss) c
    cs = fromIntegral $ conf^.ff#cellSize
    ss = conf^.ff#scale

gc :: MonadIO m => Scroller -> V2 Float -> Int -> m ()
gc sr@(Scroller ref) pos count = do
    -- putStrLn "Scroller GC"
    s <- readIORef ref
    let conf = s^.config
    let ls = sortOn (Down . distToCell conf pos . field_cellPosition)
           $ s^.ff#validCells
    let (fre, val) = splitAt count ls
    writeIORef ref $ s
        & ff#freedCells <>~ fre
        & ff#validCells .~ val

makeRenderAction :: Scroller -> Engine u RenderAction
makeRenderAction sr = readIORef (Unwrapped sr) >>= makeRA

makeRA :: ScrollerState -> Engine u RenderAction
makeRA s = do
    rr <- getRenderRegion (s^.position) conf
    let vs = cellsInRange rr $ s^.ff#validCells
    return $ renderComposition $ map renderCell vs
    where
    conf      = s^.config
    cellSize  = fromIntegral $ conf^.ff#cellSize
    viewScale = conf^.ff#scale
    cscale    = cellSize :: Float --  / viewScale

    renderCell c = renderFromAtlas $ def
        & colorMix   .~ 0
        & customPage .~ Just (s^.ff#atlasPid, regionRect c)
        & scaleX cscale
        & scaleY (negate cscale)
        & translate (fromCellPos cscale $ c^.ff#cellPosition)

    regionRect c = Rect (off+(pp*0.5)) (MkSize $ siz-pp)
        where
        pp = 1 / bs
        bs = fromIntegral <$> Unwrapped (s^.buffer.size)
        off = (fromIntegral <$> c^.ff#bufferRegion) / bs
        siz = V2 cellSize cellSize / bs

