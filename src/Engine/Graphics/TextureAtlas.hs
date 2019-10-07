{-# Language StrictData #-}
module Engine.Graphics.TextureAtlas
    ( module Engine.Graphics.TextureAtlas.Types

    , new, done
    , setupAtlas
    , getLookupAtlasLocation
    , lookupAtlasLocation
    , lookupAtlasLocations

    , assignCustomPage
    , swapCustomPage

    , addTexture

    -- , incrementalUpdate
    , fullUpdate
    ) where

import Delude
import Linear
-- import Text.Printf (printf)
import Foreign hiding (void, new)
import Graphics.GL
import Engine.Graphics.Utils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as STM

import Engine.Graphics.TextureAtlas.Types
import Engine.Graphics.TaskManager (TaskManager)
import qualified Engine.Graphics.TaskManager as TaskManager

--------------------------------------------------------------------------------

allSlotsOfSize :: AtlasLayout a -> SlotSize -> Seq QuadPath
allSlotsOfSize atlasLayout slotSize = go maxSize tree empty empty
    where
    maxSize = atlasLayout^.topSlotSize
    tree    = atlasLayout^.quadTree
    sizeDiff cur = fromEnum cur - fromEnum slotSize
    go SlotSize_8 (QuadNode {}) _ paths = paths
    go cur QuadEmpty p paths
        | cur >= slotSize = allSlotsFor (sizeDiff cur) p
        | otherwise       = p <| paths
    go _     (QuadLeaf _)  _ paths = paths
    go cur n@(QuadNode {}) p paths
        | cur <= slotSize = empty
        | otherwise
        = go (pred cur) (topLeft  n) (p |> TL) paths
       <> go (pred cur) (topRight n) (p |> TR) paths
       <> go (pred cur) (botLeft  n) (p |> BL) paths
       <> go (pred cur) (botRight n) (p |> BR) paths

allSlotsFor :: Int -> QuadPath -> Seq QuadPath
allSlotsFor n p
    | n <  0 = empty
    | n == 0 = pure p
    | otherwise
        = allSlotsFor (n-1) (p |> TL)
       <> allSlotsFor (n-1) (p |> TR)
       <> allSlotsFor (n-1) (p |> BL)
       <> allSlotsFor (n-1) (p |> BR)

emptyAtlasLayout :: SlotSize -> AtlasLayout a
emptyAtlasLayout slotSize = AtlasLayout
   { field_topSlotSize = slotSize
   , field_quadTree    = QuadEmpty
   }

insertQuad :: a -> QuadPath -> QuadTree a -> QuadTree a
insertQuad v path tree = go path tree
    where
    go Empty     _ = QuadLeaf v
    go (p :< ath) n = insertNode p ath n
    go _         _ = tree -- This is just to satisfy exhaustivite checker

    insertNode p ath n@(QuadNode tl tr bl br) = case p of
        TL -> n { topLeft  = go ath tl }
        TR -> n { topRight = go ath tr }
        BL -> n { botLeft  = go ath bl }
        BR -> n { botRight = go ath br }
    insertNode p ath _ = insertNode p ath emptyQuad

    emptyQuad = QuadNode QuadEmpty QuadEmpty QuadEmpty QuadEmpty

insertEmptySlot
    :: SlotSize -> a -> AtlasLayout a -> Maybe (QuadPath, AtlasLayout a)
insertEmptySlot slotSize v atlasLayout = case emptySlots of
    (p :< _) -> Just (p, ins p)
    _        -> Nothing
    where
    emptySlots = allSlotsOfSize atlasLayout slotSize
    ins p      = atlasLayout & quadTree %~ insertQuad v p

quadPathToOffset :: SlotSize -> QuadPath -> V2 Int
quadPathToOffset maxSlotSize path = go maxSlotSize 0 path
    where
    go SlotSize_8 !acc _ = acc
    go ss !acc@(V2 !x !y) (p :< ath) = case p of
        TL -> go pss acc                    ath
        TR -> go pss (V2 (x + ps)  y      ) ath
        BL -> go pss (V2  x       (y + ps)) ath
        BR -> go pss (V2 (x + ps) (y + ps)) ath
        where
        pss = pred ss
        ps = fromSlotSize pss
    go _ !acc _ = acc

--------------------------------------------------------------------------------

fromSlotSize :: Integral a => SlotSize -> a
fromSlotSize = fromIntegral . shift (8 :: Int) . fromEnum

toSlotSize :: Integral a => a -> SlotSize
toSlotSize a
    | a < 0     = minBound
    | otherwise = toEnum (min mx b)
    where
    mx = fromEnum (maxBound :: SlotSize)
    b = floor (logBase 2 (fromIntegral a :: Float) - 3) :: Int

fitOp :: Integral a => a -> a -> Bool
-- fitOp = (<)
fitOp = (<=)

fitSlotSize :: Integral a => a -> a -> Maybe SlotSize
fitSlotSize maxSize x
    | x < 0 || x > maxSize = Nothing
    | fitOp x 8     = Just SlotSize_8
    | fitOp x 16    = Just SlotSize_16
    | fitOp x 32    = Just SlotSize_32
    | fitOp x 64    = Just SlotSize_64
    | fitOp x 128   = Just SlotSize_128
    | fitOp x 256   = Just SlotSize_256
    | fitOp x 512   = Just SlotSize_512
    | fitOp x 1024  = Just SlotSize_1024
    | fitOp x 2048  = Just SlotSize_2048
    | fitOp x 4096  = Just SlotSize_4096
    | otherwise = Nothing

new :: MonadIO m => TaskManager -> m TextureAtlas
new tm = do
    -- maxTexUnits <- glGetInteger GL_MAX_TEXTURE_IMAGE_UNITS
    -- maxTexSize  <- glGetInteger GL_MAX_TEXTURE_SIZE
    let maxTexUnits = 16
    let maxTexSize  = 4096
    amap      <- newIORef HashMap.empty
    primary   <- newIORef =<< newAtlasPages maxTexUnits maxTexSize
    -- secondary <- newIORef =<< newAtlasPages maxTexUnits maxTexSize
    custom <- newIORef Vector.empty
    tasksChan <- newTChanIO
    return TextureAtlas
        { field_maxTextureUnits = maxTexUnits
        , field_maxTextureSize  = maxTexSize
        , field_atlasMap        = amap
        , field_primaryPages    = primary
        -- , field_secondaryPages  = secondary
        , field_customPages     = custom
        , field_tasks           = tasksChan
        , field_taskManager     = tm
        }

done :: MonadIO m => TextureAtlas -> m ()
done atlas = do
    let atlasRef l = readIORef (atlas^.l)
    mapM_ (doneTextureBuffer . view buffer) =<< atlasRef primaryPages
    mapM_ (delObject glDeleteTextures) . HashMap.keys =<< atlasRef atlasMap
    mapM_ doneTextureBuffer =<< atlasRef customPages

assignCustomPage :: MonadIO m => TextureAtlas -> TextureBuffer -> m PageId
assignCustomPage atlas buf = do
    let ref = atlas^.customPages
    atomicModifyIORef' ref $ \v -> (Vector.snoc v buf, PageId $ Vector.length v)

swapCustomPage :: MonadIO m => TextureAtlas -> PageId -> TextureBuffer -> m ()
swapCustomPage atlas pid buf = do
    let ref = atlas^.customPages
    atomicModifyIORef' ref $ \v ->
        (Vector.update v $ Vector.fromList [(unPageId pid, buf)], ())

newAtlasPages :: MonadIO m => GLint -> GLint -> m AtlasPages
newAtlasPages count pageSize = do
    tts <- replicateM (fromIntegral count) $ newAtlasPage pageSize
    return $ Vector.fromList tts

newAtlasPage :: MonadIO m => GLint -> m AtlasPage
newAtlasPage pageSize = do
    tt <- createTextureBuffer pageSize pageSize nullPtr
    let slotSize = toSlotSize pageSize
    let ss = emptyAtlasLayout slotSize
    return $ AtlasPage
        { field_buffer = tt
        , field_slots  = ss
        }

setupAtlas :: MonadIO m => TextureAtlas -> Program -> m ()
setupAtlas atlas program = do
    lrs <- fmap toList $ readIORef $ atlas^.primaryPages
    forM_ (zip [0..] lrs) $ \(i, p) -> do
        bindTextureN i program
            ("Primary[" ++ show i ++ "]")
            (p^.buffer.texture)
    cus <- fmap toList $ readIORef $ atlas^.customPages
    let s = length lrs
    forM_ (zip [0..] cus) $ \(i, p) -> do
        bindTextureN (s+i) program
            ("Custom[" ++ show i ++ "]")
            (p^.texture)

tryAddSlot :: MonadIO m
    => TextureAtlas -> SlotSize -> TextureBuffer -> m (Maybe AtlasLocation)
tryAddSlot atlas slotSize buf = do
    let aref = atlas^.primaryPages
    primary <- readIORef aref
    case go 0 empty primary of
        Nothing -> return Nothing
        Just (loc, newPrimary) -> do
            writeIORef aref newPrimary
            addLocationRequest atlas loc buf
            return (Just loc)

    where
    go pageNum bs (r :< rs) =
        case mpl of
            Nothing         -> go (pageNum+1) (bs |> r) rs
            Just (path, al) -> Just (loc, bs <> (rr <| rs))
                where
                rr  = r & slots .~ al
                loc = makeLocation maxSlotSize pageNum path buf
                maxSlotSize = toSlotSize $ atlas^.maxTextureSize
        where
        layout = r^.slots
        mpl    = insertEmptySlot slotSize () layout

    go _ _ _ = Nothing

makeLocation :: SlotSize -> Int -> QuadPath -> TextureBuffer -> AtlasLocation
makeLocation maxSlotSize pageNum path buf = AtlasLocation
    { field_page      = pageNum
    , field_offset    = locOffset
    , field_size      = locSize
    }
    where
    locOffset = quadPathToOffset maxSlotSize path
    locSize   = over each fromIntegral $ V2 (buf^.width) (buf^.height)

addLocationRequest :: MonadIO m
    => TextureAtlas -> AtlasLocation -> TextureBuffer -> m ()
addLocationRequest atlas loc buf = do
    let mref = atlas^.atlasMap
    let tex  = buf^.texture
    let tm   = field_taskManager atlas
    TaskManager.addSyncTask tm $ taskAddTexture atlas loc buf
    -- addTask atlas $ TaskAddTexture loc buf
    modifyIORef' mref $ HashMap.insert tex loc

{-
addTask :: MonadIO m
    => TextureAtlas -> AtlasTask -> m ()
addTask atlas task = do
    let chan = atlas^.tasks
    writeTChanIO chan task
-}

addTexture :: MonadIO m => TextureAtlas -> TextureBuffer -> m ()
addTexture atlas buf = whenNothingM_ lookupLoc $ do
    case mSlotSize of
        Nothing -> do
            logOnceFor tex "Unable to add texture to atlas. Slot size too big."
            return ()
        Just slotSize -> do
            -- print texSize
            -- print slotSize
            mLoc <- tryAddSlot atlas slotSize buf
            case mLoc of
                Nothing -> do
                    logOnceFor tex $ "Unable to add texture to atlas. " <>
                                     "No more empty slots left of that size."
                    return ()
                Just _ -> do
                    -- let ss x = show x :: String
                    -- putStrLn ((printf "%15s %15s %15s"
                        -- (ss $ l^.offset) (ss slotSize) (ss texSize)) :: String)
                    return ()
    where
    lookupLoc  = HashMap.lookup tex <$> readIORef (atlas^.atlasMap)
    tex        = buf^.texture
    maxTexSize = atlas^.maxTextureSize
    texSize    = over each fromIntegral $ V2 (buf^.width) (buf^.height)
    mSlotSize  = fitSlotSize maxTexSize =<< maximumOf traverse texSize

logOnceFor :: MonadIO m => a -> Text -> m ()
logOnceFor _what msg = putTextLn msg

getLookupAtlasLocation :: MonadIO m
    => TextureAtlas -> m (Texture -> Maybe AtlasLocation)
getLookupAtlasLocation atlas = do
    am <- readIORef (atlas^.atlasMap)
    return $ flip HashMap.lookup am

lookupAtlasLocation :: MonadIO m
    => TextureAtlas -> Texture -> m (Maybe AtlasLocation)
lookupAtlasLocation atlas tex = HashMap.lookup tex <$> readIORef (atlas^.atlasMap)

lookupAtlasLocations :: MonadIO m
    => TextureAtlas -> [Texture] -> m [Maybe AtlasLocation]
lookupAtlasLocations atlas ts = do
    amap <- readIORef (atlas^.atlasMap)
    return $ map (flip HashMap.lookup amap) ts

{-
incrementalUpdate :: MonadIO m => TextureAtlas -> m Bool -> m ()
incrementalUpdate atlas timesUp = go
    where
    chan = atlas^.tasks
    go = do
        mTask <- tryReadTChanIO chan
        whenJust mTask $ \task -> do
            performTask atlas task
            unlessM timesUp go

performTask :: MonadIO m => TextureAtlas -> AtlasTask -> m ()
performTask atlas = \case
    TaskAddTexture loc buf -> liftIO $ taskAddTexture atlas loc buf
-}

taskAddTexture :: TextureAtlas -> AtlasLocation -> TextureBuffer -> IO ()
taskAddTexture atlas loc buf = do
    primary <- readIORef $ atlas^.primaryPages
    let mPage = Vector.indexM primary $ loc^.page
    whenJust mPage $ \atlasPage -> do
        let off = fromIntegral <$> loc^.offset
        copyAtOffset off buf (atlasPage^.buffer)

copyAtOffset
    :: MonadIO m
    => V2 Int32      -- ^ target offset in pixels
    -> TextureBuffer -- ^ source texture
    -> TextureBuffer -- ^ target texture
    -> m ()
copyAtOffset pos source target = do

    let srcName   = source^.texture
    let dstName   = target^.texture
    let dstX      = pos^._x
    let dstY      = pos^._y
    let srcWidth  = source^.width
    let srcHeight = source^.height

    glCopyImageSubData
        srcName GL_TEXTURE_2D 0 0 0 0
        dstName GL_TEXTURE_2D 0 dstX dstY 0
        srcWidth srcHeight 1

    {-
    withTextureBuffer source $ do
        let targetTexture = target^.buffer.texture
        let V2 x y = fromIntegral <$> loc^.offset
        let V2 w h = fromIntegral <$> loc^.size
        glBindTexture GL_TEXTURE_2D targetTexture
        glCopyTexSubImage2D GL_TEXTURE_2D 0 x y 0 0 w h
        glBindTexture GL_TEXTURE_2D noTexture
        -}

fullUpdate :: MonadIO m => TextureAtlas -> m ()
fullUpdate atlas = return () -- incrementalUpdate atlas (return False)

--------------------------------------------------------------------------------

newTChanIO :: MonadIO m => m (TChan a)
newTChanIO = liftIO STM.newTChanIO

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = liftIO . atomically . STM.writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = liftIO . atomically . STM.tryReadTChan

