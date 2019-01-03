module Engine.Graphics.Draw.Shape (initDrawShapes) where

import Delude
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (toList)
import Foreign (nullPtr)
import Prelude as String (unlines)

import Graphics.GL
import Engine.Graphics.Utils

import Linear
{- import Diagrams.TwoD.Transform (rotateBy) -}

--------------------------------------------------------------------------------

initDrawShapes :: MonadIO m => m (DrawBatch ShapesBatch)
initDrawShapes = do
    program <- createProgram vertexShader fragmentShader
    vao <- glCreateVertexArray

    let vs = concat $ replicate maxBatchSize $ concatMap toList defVertices
    let is = concatMap makeIndices [0 .. fromIntegral $ maxBatchSize-1]
    verticesBuffer <- createBuffer GL_ARRAY_BUFFER (vs :: [Float])
    colorBuffer    <- glCreateBuffer
    radiusBuffer   <- glCreateBuffer
    borderBuffer   <- glCreateBuffer
    modelXBuffer   <- glCreateBuffer
    modelYBuffer   <- glCreateBuffer
    indecesBuffer  <- createBuffer GL_ELEMENT_ARRAY_BUFFER (is :: [Word32])

    let setup proj = do
            glEnable GL_LINE_SMOOTH
            glUseProgram program
            glBindVertexArray vao
            bindArrayBuffer program "VertexPosition" verticesBuffer 2
            bindArrayBuffer program "Color"          colorBuffer    4
            bindArrayBuffer program "Radius"         radiusBuffer   4
            bindArrayBuffer program "Border"         borderBuffer   4
            bindArrayBuffer program "ModelX"         modelXBuffer   3
            bindArrayBuffer program "ModelY"         modelYBuffer   3
            setUniform program "ProjectionMatrix" proj

    let draw r = do
            let st    = r^.shapeType
            {- let vs    = shapeVertices st -}
            let ct    = length defVertices
            let cs    = convColor ct . view color $ r
            let rads  = fromVecCt ct $ radiusVec st
            let bs    = fromVecCt ct $ border
            let trans = mkModelVecXY . view modelTransform $ r
            let mxs   = fromVecCt ct . view _x $ trans
            let mys   = fromVecCt ct . view _y $ trans
            -- setBufferData verticesBuffer GL_ARRAY_BUFFER (vs   :: [Float])
            setBufferData radiusBuffer   GL_ARRAY_BUFFER (rads :: [Float])
            setBufferData borderBuffer   GL_ARRAY_BUFFER (bs   :: [Float])
            setBufferData colorBuffer    GL_ARRAY_BUFFER (cs   :: [Float])
            setBufferData modelXBuffer   GL_ARRAY_BUFFER (mxs  :: [Float])
            setBufferData modelYBuffer   GL_ARRAY_BUFFER (mys  :: [Float])
            -- glDrawArrays GL_TRIANGLE_FAN 0 (fromIntegral ct)
            let count = fromIntegral $ ct * 6
            glBindBuffer GL_ELEMENT_ARRAY_BUFFER indecesBuffer
            glDrawElements GL_TRIANGLES count GL_UNSIGNED_INT nullPtr

    return $ \proj xs -> setup proj >> mapM_ draw xs
    where
        {-
    shape program r start end = do
        glDrawArrays GL_TRIANGLE_FAN start end
        -- setUniform program "Color" (fromColor $ r^.borderColor)
        -- glDrawArrays GL_LINE_LOOP start end
        -}

    maxBatchSize :: Int
    maxBatchSize = 16384

    vertexShader = String.unlines $ shaderVersion :
        [ "in vec2 VertexPosition;"
        , "in vec4 Color;"
        , "in vec4 Radius;"
        , "in vec4 Border;"
        , "in vec3 ModelX;"
        , "in vec3 ModelY;"
        , "uniform mat4 ProjectionMatrix;"
        , "out highp vec2 vPosition;"
        , "out highp vec4 vColor;"
        , "out highp vec4 vRadius;"
        , "out highp vec4 vBorder;"
        , "void main(void){"
        , "  mat4 MM = mat4(1.0);"
        , "  MM[0] = vec4(ModelX.xy, 0, ModelX.z);"
        , "  MM[1] = vec4(ModelY.xy, 0, ModelY.z);"
        , "  mat4 Matrix = ProjectionMatrix * transpose(MM);"
        , "  gl_Position = Matrix * vec4(VertexPosition, 0, 1);"
        , "  vPosition = VertexPosition*2;"
        , "  vRadius = Radius;"
        , "  vBorder = Border;"
        , "  vColor = Color;"
        , "}" ]

    fragmentShader = String.unlines $ shaderVersion :
        [ "in highp vec2 vPosition;"
        , "in highp vec4 vColor;"
        , "in highp vec4 vRadius;"
        , "in highp vec4 vBorder;"
        , "out vec4 FragColor;"
        , "void main(void){"
        , "  vec2  p = vPosition;"
        , "  float delta = fwidth(length(p));"
        , "  FragColor = vColor;"
        , "  float r = 0;"
        , "  vec2  c = vec2(0);"
        , "  float v = 0;"
        -- , "  float rr= 0;"
        -- , "  vec2  cb= vec2(0);"
        -- , "  float vb= 0;"
        -- , "  float bx = 0; float by = 0;"

        , "  vec4  b = 1.0 - vBorder;"
        , "  if (p.x < b[0] && p.x > -b[2]"
        , "   && p.y < b[1] && p.y > -b[3]) FragColor.a = 0;"

        , renderCorner 0 (0, 1) (True, True)
        , renderCorner 1 (2, 1) (False, True)
        , renderCorner 2 (2, 3) (False, False)
        , renderCorner 3 (0, 3) (True, False)
        , "}" ]

    renderCorner :: Int -> (Int, Int) -> (Bool, Bool) -> String
    renderCorner r (_bx, _by) (cx, cy) = concat [""
        -- , "  bx= vBorder[", show bx, "];"
        -- , "  by= vBorder[", show by, "];"
        , "  r = vRadius[",show r,"];"
        , "  c = vec2(",sx,"(1.0-r), ",sy,"(1.0-r));"
        , "  v = smoothstep(r - delta, r + delta, length(p-c));"
        -- , "  rr= r > 0 ? max(bx,by) : 0;"
        -- , "  cb= vec2(",sx,"(1.0-rr-bx), ",sy,"(1.0-rr-by));"
        -- , "  vb= smoothstep(rr - delta, rr + delta, length(p-cb));"
     -- , "  if (p.x ",ox," cb.x && p.y ",oy," cb.y"
     -- , "   && p.x ",ox,"    0 && p.y ",oy,"    0) FragColor.a  = vb;"
     -- , "  if (p.x ",ox,"  c.x && p.y ",oy,"  c.y) FragColor.a  = vb-v;" ]
        , "  if (p.x ",ox,"  c.x && p.y ",oy,"  c.y) FragColor.a  = 1 -v;" ]
        where
        sx = if cx then "+" else "-"
        sy = if cy then "+" else "-"
        ox = if cx then ">" else "<"
        oy = if cy then ">" else "<"

    border :: V4 Float
    -- border = 0.10 -- V4 0.2 0.01 0.5 0.4
    border = 1 -- V4 0.2 0.01 0.5 0.4

    radiusVec :: SimpleShape -> V4 Float
    radiusVec = \case
        SimpleSquare -> V4 0 0 0 0
        -- SimpleCircle -> V4 0 1 0.2 0.4
        SimpleCircle -> 1

    {- shapeVertices = \case -}
        {- SimpleSquare -> squareVertices -}
        {- SimpleCircle -> squareVertices -}
        -- SimpleCircle -> circleVertices

    defVertices :: [V2 Float]
    defVertices = map (*0.5)
        [ V2   1  (-1)
        , V2 (-1) (-1)
        , V2   1    1
        , V2 (-1)   1 ]

    {- squareVertices :: [Float] -}
    {- squareVertices = map (*0.5) $ -}
        {- [ -1, -1 -}
        {- ,  1, -1 -}
        {- ,  1,  1 -}
        {- , -1,  1 ] -}

    {- circleVertices :: [Float] -}
    {- circleVertices -}
        {- = map (*0.5) $ concat $ map toList -}
        {- $ map (\x -> rotateBy (x/circleN) unitX) [1..circleN] -}

    {- circleN = 16 -}

    {- unitX :: V2 Float -}
    {- unitX = unit _x -}

    convColor :: Int -> AlphaColor -> [Float]
    convColor ct = concatMap (toList . fromAlphaColor) . replicate ct

    fromVecCt :: (Foldable t, Real x) => Int -> t x -> [Float]
    fromVecCt c = map realToFrac . concat . replicate c . toList

    makeIndices :: Word32 -> [Word32]
    makeIndices i = map (\x ->x+i*4) [0, 1, 2, 2, 1, 3]


