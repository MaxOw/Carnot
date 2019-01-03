module Engine.Graphics.Draw.Texture (initDrawTextures) where

import Protolude
import Prelude (unlines)

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.GL
import Engine.Graphics.Utils

--------------------------------------------------------------------------------

initDrawTextures :: MonadIO m => m (DrawBatch TexturesBatch)
initDrawTextures = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER vertices
    texCoordBuffer <- createBuffer GL_ARRAY_BUFFER texCoords
    -- indecesBuffer  <- createBuffer ELEMENT_ARRAY_BUFFER indices
    let setup = do
          glEnable GL_BLEND
          -- glEnable GL_DEPTH_TEST
          glDisable GL_DEPTH_TEST
          glBlendFuncSeparate
              GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
              GL_ONE       GL_ONE_MINUS_SRC_ALPHA

          glUseProgram program
          glBindVertexArray vao
          bindArrayBuffer  program "VertexPosition" verticesBuffer 3
          bindArrayBuffer  program "TexCoord"       texCoordBuffer 2

          -- glBindBuffer     ELEMENT_ARRAY_BUFFER indecesBuffer

    let draw proj r = do
          bindTexture0 program "Texture" $ r^.textureId
          -- setBufferData texCoordBuffer GL_ARRAY_BUFFER ts
          setUniform program "ModelMatrix" (mkMatHomo2 $ r^.modelTransform)
          setUniform program "ProjectionMatrix" proj
          -- glDrawElements TRIANGLES 6 UNSIGNED_SHORT nullPtr
          -- glDrawElements TRIANGLE_STRIP 4 UNSIGNED_SHORT nullPtr
          glDrawArrays GL_TRIANGLE_STRIP 0 4

    return $ \proj b -> setup >> mapM_ (draw proj) b
    where
    vertexShader = unlines $ shaderVersion :
        [ "in vec3 VertexPosition;"
        , "in vec2 TexCoord;"
        , "uniform mat4 ModelMatrix;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec2 vTexCoord;"
        , "void main(void){"
        , "gl_Position = ProjectionMatrix * ModelMatrix * " ++
            "vec4(VertexPosition, 1.0);"
        , "vTexCoord   = TexCoord;"
        , "}" ]
    fragmentShader = unlines $ shaderVersion :
        [ "uniform sampler2D Texture;"
        , "in highp vec2 vTexCoord;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "FragColor = texture2D(Texture, vTexCoord);"
        , "}" ]

    vertices :: [Float]
    vertices = map (*0.5)
        [  1, -1,  0
        , -1, -1,  0
        ,  1,  1,  0
        , -1,  1,  0 ]

    texCoords :: [Float]
    texCoords =
        [ 1,  1
        , 0,  1
        , 1,  0
        , 0,  0 ]
