module Engine.Graphics.TextureCache
    ( TextureCache
    , newCache
    , retriveOrAdd
    )
    where

import Delude
import qualified Data.IntMap as IntMap
import qualified Diagrams.TwoD.Transform as T

import Graphics.GL
import Engine.Common.Types
import Engine.Graphics.Types
import Engine.Graphics.Utils
import Engine.Graphics.TextureAtlas
import Engine.Graphics.RenderAction
import Engine.Lens.Utils

import Engine.Graphics.TextureCache.Types
import qualified Engine.Graphics.TaskManager as TaskManager
import Engine.Graphics.TaskManager (TaskManager)

--------------------------------------------------------------------------------

newCache :: MonadIO m => TaskManager -> TextureAtlas -> m TextureCache
newCache tm atlas = do
    ref <- newIORef mempty
    return $ TextureCache
        { field_atlas       = atlas
        , field_cache       = ref
        , field_taskManager = tm
        }

retriveOrAdd :: MonadIO m
    => TextureCache
    -> Size Int32
    -> Int -- ^ Cache key
    -> Maybe AlphaColor
    -> IO DrawBatch
    -> (Mat4 -> DrawBatch -> IO ())
    -> m RenderAction
retriveOrAdd cache ss@(Size sw sh) hid mCol batchAsync drawToCache = do
    cmap <- readIORef $ cache^.ff#cache
    case IntMap.lookup hid cmap of
        Nothing -> addToCache
        Just tx -> lookupAtlasLocation (cache^.ff#atlas) tx >>= \case
            Nothing -> addToCache -- TODO: don't redraw, just re-add to atlas
            Just _l -> return $ makeRenderAction tx
    where
    addToCache = do
        buf <- createTextureBuffer sw sh nullPtr
        let projM = orthoProjectionFor (fmap realToFrac ss) $ def
                & boxAlign .~ MiddleLeft
        let drawSync = mkDrawSync buf projM
        let tm = cache^.ff#taskManager
        TaskManager.addTask tm batchAsync drawSync
        let tex = buf^.texture
        modifyIORef (cache^.ff#cache) $ IntMap.insert hid tex
        addTexture (cache^.ff#atlas) buf
        return $ makeRenderAction tex

    mkDrawSync buf projM bs = withTextureBuffer buf $ do
        glClearColor 0 0 0 0
        glClear GL_COLOR_BUFFER_BIT
        liftIO $ drawToCache projM bs

    applyColor = case mCol of
        Nothing -> set colorMix 0
        Just cl -> set color cl

    makeRenderAction tx = renderFromAtlas $ def
        & textureId .~ Just tx
        & applyColor
        & T.translate (V2 (0.5) 0)
        & T.scaleX (realToFrac sw)
        & T.scaleY (negate $ realToFrac sh)

