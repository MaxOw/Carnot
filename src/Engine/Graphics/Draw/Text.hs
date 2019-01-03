module Engine.Graphics.Draw.Text (initDrawText) where

import Protolude
import Prelude (unlines)

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))

import Graphics.GL
import Engine.Graphics.Utils

--------------------------------------------------------------------------------

initDrawText :: MonadIO m => m (DrawBatch TextBatch)
initDrawText = do
    program <- createProgram vertexShader fragmentShader
    vao     <- glCreateVertexArray
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER vertices
    texCoordBuffer <- createBuffer GL_ARRAY_BUFFER texCoords
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

    let draw proj r = do
          bindTexture0 program "Texture" $ r^.textureId
          -- setBufferData texCoordBuffer GL_ARRAY_BUFFER ts
          setUniform program "FontColor" (fromColor $ r^.color)
          setUniformMatrix program "ModelViewMatrix"  (r^.modelMatrix)
          setUniformMatrix program "ProjectionMatrix" proj
          glDrawArrays GL_TRIANGLE_STRIP 0 4

    return $ \proj b -> setup >> mapM_ (draw proj) b
    where
    vertexShader = unlines $ shaderVersion :
        [ "in vec3 VertexPosition;"
        , "in vec2 TexCoord;"
        , "uniform mat4 ModelViewMatrix;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec2 vTexCoord;"
        , "void main(void){"
        , "gl_Position = ProjectionMatrix * ModelViewMatrix * " ++
            "vec4(VertexPosition, 1.0);"
        , "vTexCoord   = TexCoord;"
        , "}" ]
    fragmentShader = unlines $ shaderVersion :
        [ "uniform sampler2D Texture;"
        , "uniform vec4 FontColor;"
        , "in highp vec2 vTexCoord;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "FragColor.rgb = FontColor.rgb;"
        , "FragColor.a   = texture2D(Texture, vTexCoord).r;"
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
