{-# Language StrictData #-}
module Engine.Graphics.TextureAtlas
    ( module Engine.Graphics.TextureAtlas.Types

    , newAtlas, done
    , setupAtlas
    , lookupAtlasLocation
    , lookupAtlasLocations

    , assignCustomPage
    , swapCustomPage

    -- , createTextureOrigin
    , addTexture

    , incrementalUpdate
    , fullUpdate
    ) where

import Delude
-- import Linear
-- import Text.Printf (printf)
import Foreign hiding (void)
import Graphics.GL
-- import Engine.Graphics.Types
import Engine.Graphics.Utils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
-- import qualified Data.Sequence as Seq
-- import Data.IORef
import Control.Concurrent.STM.TChan (TChan)
import qualified Control.Concurrent.STM.TChan as STM

import Engine.Graphics.TextureAtlas.Types

--------------------------------------------------------------------------------

{-
createTextureOrigin :: MonadIO m => DynamicImage -> m TextureOrigin
createTextureOrigin dyn = do
    let Image w h bytes = convertRGBA8 dyn
    buf <- liftIO $ unsafeWith bytes $ \ptr ->
        createTextureBuffer (fromIntegral w) (fromIntegral h) (castPtr ptr)
    return $ TextureOrigin
        { textureOrigin_buffer      = buf
        , textureOrigin_textureSize = V2 w h
        }
-}

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
   { atlasLayout_topSlotSize = slotSize
   , atlasLayout_quadTree    = QuadEmpty
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

-- canInsertSlot :: AtlasLayout a -> SlotSize -> Bool
-- canInsertSlot atlasLayout = not . null . allSlotsOfSize atlasLayout

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

{-
testAtlasLayout :: AtlasLayout a
testAtlasLayout = emptyAtlasLayout SlotSize_1024

-- testOff :: V2 Int
-- testOff = quadPathToOffset SlotSize_1024 (Seq.fromList [BR, TR, TL])

testL :: Seq QuadPath
-- testL = toList $ allSlotsOfSize testAtlasLayout SlotSize_8
testL = allSlotsOfSize testAtlasLayout SlotSize_512

Just ta = insertEmptySlot SlotSize_512 () testAtlasLayout
Just tb = insertEmptySlot SlotSize_16 () (snd ta)
xx = flip allSlotsOfSize SlotSize_16 (snd tb)
-}

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

-- newtype TextureName = TextureName Text
-- type TextureName = Text

newAtlas :: MonadIO m => m TextureAtlas
newAtlas = do
    -- maxTexUnits <- glGetInteger GL_MAX_TEXTURE_IMAGE_UNITS
    -- maxTexSize  <- glGetInteger GL_MAX_TEXTURE_SIZE
    let maxTexUnits = 16
    let maxTexSize  = 4096
    amap      <- newRef HashMap.empty
    primary   <- newRef =<< newAtlasPages maxTexUnits maxTexSize
    -- secondary <- newRef =<< newAtlasPages maxTexUnits maxTexSize
    custom <- newRef Vector.empty
    tasksChan <- newTChanIO
    return TextureAtlas
        { textureAtlas_maxTextureUnits = maxTexUnits
        , textureAtlas_maxTextureSize  = maxTexSize
        , textureAtlas_atlasMap        = amap
        , textureAtlas_primaryPages    = primary
        -- , textureAtlas_secondaryPages  = secondary
        , textureAtlas_customPages     = custom
        , textureAtlas_tasks           = tasksChan
        }

done :: MonadIO m => TextureAtlas -> m ()
done atlas = do
    let atlasRef l = readRef (atlas^.l)
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
        { atlasPage_buffer = tt
        , atlasPage_slots  = ss
        }

setupAtlas :: MonadIO m => TextureAtlas -> Program -> m ()
setupAtlas atlas program = do
    lrs <- fmap toList $ readRef $ atlas^.primaryPages
    forM_ (zip [0..] lrs) $ \(i, p) -> do
        bindTextureN i program
            ("Primary[" ++ show i ++ "]")
            (p^.buffer.texture)
    cus <- fmap toList $ readRef $ atlas^.customPages
    let s = length lrs
    forM_ (zip [0..] cus) $ \(i, p) -> do
        bindTextureN (s+i) program
            ("Custom[" ++ show i ++ "]")
            (p^.texture)

tryAddSlot :: MonadIO m
    => TextureAtlas -> SlotSize -> TextureBuffer -> m (Maybe AtlasLocation)
tryAddSlot atlas slotSize buf = do
    let aref = atlas^.primaryPages
    primary <- readRef aref
    case go 0 empty primary of
        Nothing -> return Nothing
        Just (loc, newPrimary) -> do
            writeRef aref newPrimary
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
    { atlasLocation_page      = pageNum
    , atlasLocation_offset    = locOffset
    , atlasLocation_size      = locSize
    -- , atlasLocation_texCoords = makeTexCoords maxSlotSize locOffset locSize
    }
    where
    locOffset = quadPathToOffset maxSlotSize path
    locSize   = over each fromIntegral $ V2 (buf^.width) (buf^.height)

{-
makeTexCoords :: SlotSize -> V2 Int -> V2 Int -> Rect Float
makeTexCoords maxSlotSize locOffset locSize = Rect vo (MkSize vs)
    where
    ss = fromIntegral (fromSlotSize maxSlotSize :: Int)
    vo = ((fromIntegral <$> locOffset) + 0.5) ^/ ss
    vs = ((fromIntegral <$> locSize)   - 1.0) ^/ ss
-}

addLocationRequest :: MonadIO m
    => TextureAtlas -> AtlasLocation -> TextureBuffer -> m ()
addLocationRequest atlas loc buf = do
    let mref = atlas^.atlasMap
    let tex  = buf^.texture
    addTask atlas $ TaskAddTexture loc buf
    modifyRef mref $ HashMap.insert tex loc

addTask :: MonadIO m
    => TextureAtlas -> AtlasTask -> m ()
addTask atlas task = do
    let chan = atlas^.tasks
    writeTChanIO chan task

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
    lookupLoc  = HashMap.lookup tex <$> readRef (atlas^.atlasMap)
    tex        = buf^.texture
    maxTexSize = atlas^.maxTextureSize
    texSize    = over each fromIntegral $ V2 (buf^.width) (buf^.height)
    mSlotSize  = fitSlotSize maxTexSize =<< maximumOf traverse texSize

logOnceFor :: MonadIO m => a -> Text -> m ()
logOnceFor _what msg = putStrLn msg

{-
lookupTexture :: MonadIO m => TextureAtlas -> TextureOrigin -> m AtlasLocation
lookupTexture atlas origin = do
    amap <- readRef $ atlas^.atlasMap
    let otex = origin^.buffer.texture
    let mLoc = HashMap.lookup otex amap
    case mLoc of
        Nothing  -> addTexture atlas origin
        Just loc -> return loc
-}

lookupAtlasLocation :: MonadIO m
    => TextureAtlas -> Texture -> m (Maybe AtlasLocation)
lookupAtlasLocation atlas tex = HashMap.lookup tex <$> readRef (atlas^.atlasMap)

lookupAtlasLocations :: MonadIO m
    => TextureAtlas -> [Texture] -> m [Maybe AtlasLocation]
lookupAtlasLocations atlas ts = do
    amap <- readRef (atlas^.atlasMap)
    return $ map (flip HashMap.lookup amap) ts

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
    TaskAddTexture loc buf -> do
        primary <- readRef $ atlas^.primaryPages
        let mPage = Vector.indexM primary $ loc^.page
        whenJust mPage $ \atlasPage -> do
            let atlasTexture = atlasPage^.buffer.texture
            withTextureBuffer buf $ do
                let V2 x y = fromIntegral <$> loc^.offset
                let V2 w h = fromIntegral <$> loc^.size
                glBindTexture GL_TEXTURE_2D atlasTexture
                glCopyTexSubImage2D GL_TEXTURE_2D 0 x y 0 0 w h
                glBindTexture GL_TEXTURE_2D noTexture

fullUpdate :: MonadIO m => TextureAtlas -> m ()
fullUpdate atlas = incrementalUpdate atlas (return False)

--------------------------------------------------------------------------------

newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref = liftIO . writeIORef ref

modifyRef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyRef ref = liftIO . modifyIORef' ref

--------------------------------------------------------------------------------

newTChanIO :: MonadIO m => m (TChan a)
newTChanIO = liftIO STM.newTChanIO

writeTChanIO :: MonadIO m => TChan a -> a -> m ()
writeTChanIO chan = liftIO . atomically . STM.writeTChan chan

tryReadTChanIO :: MonadIO m => TChan a -> m (Maybe a)
tryReadTChanIO = liftIO . atomically . STM.tryReadTChan

