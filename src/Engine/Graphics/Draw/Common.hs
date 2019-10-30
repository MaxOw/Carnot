{-# Language ScopedTypeVariables #-}
module Engine.Graphics.Draw.Common (makeInitDrawCall) where

import Delude
import Data.Proxied (sizeOfProxied)
import Engine.Graphics.Buffer
import Engine.Graphics.Utils
import Foreign (nullPtr)

import Graphics.GL
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Generic as Vector

makeInitDrawCall
    :: forall i. ArrayBufferItem i
    => String
    -> String
    -> IO (Storable.Vector i -> (Program -> IO ()) -> IO ())
makeInitDrawCall vertexShader fragmentShader = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    let cs = sizeOfProxied (Proxy :: Proxy i)
    let maxBatchSize = div (5*1024*1024) (cs*4)
    -- putStrLn $ "maxBatchSize: " <> show maxBatchSize
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
            setArrayBufferData arrayBuffer h
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
            let elemCount = fromIntegral $ 6 * (div (Vector.length h) 4)
            glDrawElements GL_TRIANGLES elemCount GL_UNSIGNED_INT nullPtr
    where
    go maxBatchSize fullBatch act =
        let (h, t) = Vector.splitAt (maxBatchSize*4) fullBatch
        in unless (Vector.null h) (act h >> go maxBatchSize t act)

    defVertices :: [V2 Float]
    defVertices = map (*0.5)
        [ V2   1  (-1)
        , V2 (-1) (-1)
        , V2   1    1
        , V2 (-1)   1 ]

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]

