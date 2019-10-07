{-# Options_GHC -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
module Experiments.Common
    ( Benchmark, mkBench

    , Context
    , withBuffers

    , orthoMat
    , makeInitDrawCall
    , newAtlas

    , loadImgToAtlas
    ) where

import Delude
import Data.Proxied (sizeOfProxied)
import Data.Bits ((.|.))
import Foreign (nullPtr)
import Criterion.Main
import Foreign.Storable (Storable)

import Graphics.GL
import Engine.Common.Types (pattern Size)
import Engine.Graphics (Mat4, orthoProjectionFor)
import Engine.Context (Context)
import Engine.Graphics.Buffer
import Engine.Graphics.Utils
import Engine.Graphics.TextureAtlas
import qualified Engine.Context as Context

import qualified Data.Vector.Storable as Storable
-- import Data.Vector (Vector)
import qualified Data.Vector.Generic as Vector

import qualified Engine.Graphics.TaskManager as TaskManager
import qualified Engine.Graphics.TextureAtlas as Atlas
import Engine.Graphics.TextureAtlas (TextureAtlas)

import Codec.Picture (Image(..), convertRGBA8)

instance NFData Context

mkBench :: NFData a => String -> IO a -> (a -> IO ()) -> Benchmark
mkBench n i r = env i $ \st -> bench n $ whnfIO (r st)

withBuffers :: Context -> IO a -> IO a
withBuffers cx act = do
    (w, h) <- Context.getFramebufferSize cx
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClearColor 1 1 1 1
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    -- glClear GL_COLOR_BUFFER_BIT --  .|. GL_DEPTH_BUFFER_BIT)

    ret <- act

    Context.swapBuffers cx
    return ret

newAtlas :: IO TextureAtlas
newAtlas = Atlas.new =<< TaskManager.new

orthoMat :: Context -> IO Mat4
orthoMat cx = do
    canvasSize <- Context.getFramebufferSize cx
    let wh = over each fromIntegral canvasSize
    return $ orthoProjectionFor (uncurry Size wh) def

makeInitDrawCall
    :: forall i a. ArrayBufferItem i
    => Storable a
    => String
    -> String
    -> (a -> Storable.Vector i)
    -> IO (Storable.Vector a -> (Program -> IO ()) -> IO ())
makeInitDrawCall vertexShader fragmentShader toItems = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    let cs = sizeOfProxied (Proxy :: Proxy i)
    let maxBatchSize = div (5*1024*1024) (cs*4)
    putStrLn $ "maxBatchSize: " <> show maxBatchSize
    let vs = concat $ replicate maxBatchSize $ concatMap toList defVertices
    let is = concatMap makeIndices [0 .. fromIntegral $ maxBatchSize-1]
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER (vs :: [Float])
    arrayBuffer    <- createArrayBuffer program
    indecesBuffer  <- createBuffer GL_ELEMENT_ARRAY_BUFFER (is :: [Word32])
    return $ \fullBatch setUniforms -> do
        glUseProgram program
        glBindVertexArray vao
        bindArrayBuffer program "VertexPosition" verticesBuffer 2
        bindArrayBufferPointers arrayBuffer

        setUniforms program

        go maxBatchSize fullBatch $ \h -> do
            setArrayBufferData arrayBuffer $ Vector.concatMap toItems h
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
            let elemCount = fromIntegral $ 6 * Vector.length h
            glDrawElements GL_TRIANGLES elemCount GL_UNSIGNED_INT nullPtr
    where
    go maxBatchSize fullBatch act =
        let (h, t) = Vector.splitAt maxBatchSize fullBatch
        in unless (Vector.null h) (act h >> go maxBatchSize t act)

    defVertices :: [V2 Float]
    defVertices = map (*0.5)
        [ V2   1  (-1)
        , V2 (-1) (-1)
        , V2   1    1
        , V2 (-1)   1 ]

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]


loadImgToAtlas :: TextureAtlas -> FilePath -> IO (Maybe Img)
loadImgToAtlas atlas path = loadImageSync path >>= \case
    Nothing  -> return Nothing
    Just img -> do
        buf <- createTextureBufferFrom img
        addTexture atlas buf
        let (Image w h _) = convertRGBA8 img
        let tex = buf^.texture
        return $ Just $ mkImg tex (Size w h)

