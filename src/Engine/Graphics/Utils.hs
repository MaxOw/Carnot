{-# Language RankNTypes #-}
module Engine.Graphics.Utils
    ( module Engine.Graphics.Utils
    ) where

import Delude hiding (identity)
import qualified Unsafe
-- import Control.Concurrent.Chan (Chan, writeChan)
import Prelude (String)
import Foreign hiding (void)
import Foreign.C.String
import Data.Array.Storable (newListArray, withStorableArray)
import Data.Vector.Storable (unsafeWith)
import Linear
import Codec.Picture
    (DynamicImage(..), Image(..), PixelRGBA8, convertRGBA8, readImage)
-- import System.Directory (findFile)

import Diagrams.Transform
import Diagrams.Transform.Matrix

import Graphics.GL
import Engine.Backend.Types
import Engine.Graphics.Types as Engine.Graphics.Utils
import Data.Colour.SRGB
import Data.Colour (AlphaColour)
import qualified Data.Colour as Color

--------------------------------------------------------------------------------

fromColor :: Color -> V4 Float
fromColor c = V4 r g b 1
    where
    RGB r g b = toSRGB c

fromAlphaColor :: AlphaColor -> V4 Float
fromAlphaColor c = V4 r g b a
    where
    RGB r g b = toSRGB $ pureColor Color.black c
    a = Color.alphaChannel c

pureColor :: (Ord a, Fractional a) => Colour a -> AlphaColour a -> Colour a
pureColor d ac
    | a > 0     = Color.darken (recip a) (Color.over ac Color.black)
    | otherwise = d
    where
    a = Color.alphaChannel ac

--------------------------------------------------------------------------------

shaderVersion :: String
shaderVersion = "#version 450"

noBuffer :: Buffer
noBuffer = 0

noFramebuffer :: Framebuffer
noFramebuffer = 0

nullTexture :: Ptr w
nullTexture = nullPtr

nullGLPtr :: Ptr w
nullGLPtr = nullPtr

glCreateBuffer :: MonadIO m => m Buffer
glCreateBuffer = genObject glGenBuffers

glCreateFramebuffer :: MonadIO m => m Framebuffer
glCreateFramebuffer = genObject glGenFramebuffers

glCreateTexture :: MonadIO m => m Texture
glCreateTexture = genObject glGenTextures

glCreateVertexArray :: MonadIO m => m VAO
glCreateVertexArray = genObject glGenVertexArrays

createBuffer :: (Storable v, MonadIO m) => GLenum -> [v] -> m Buffer
createBuffer bufferType bufferData = do
    buf <- glCreateBuffer
    setBufferData buf bufferType bufferData
    return buf

setBufferData :: (Storable v, MonadIO m) => Buffer -> GLenum -> [v] -> m ()
setBufferData buf bufferType bufferData = do
    glBindBuffer bufferType buf
    withArrayGL bufferData $ \size' ptr ->
        glBufferData bufferType size' ptr GL_STATIC_DRAW
    glBindBuffer bufferType noBuffer

bindArrayBuffer :: MonadIO m => Program -> String -> Buffer -> GLint -> m ()
bindArrayBuffer program name buf size' = do
    glBindBuffer GL_ARRAY_BUFFER buf
    loc <- getAttribLocation program name
    glVertexAttribPointer (fromIntegral loc) size' GL_FLOAT GL_FALSE 0 nullGLPtr
    glEnableVertexAttribArray (fromIntegral loc)

withArrayGL :: (Storable v, MonadIO m)
    => [v] -> (GLsizeiptr -> Ptr a -> IO ()) -> m ()
withArrayGL []       _ = return ()
withArrayGL ls@(l:_) f = do
    let len = length ls
    let n = fromIntegral $ len * sizeOf l
    arr <- liftIO $ newListArray (0, len - 1) ls
    liftIO $ withStorableArray arr $ \ptr -> f n (castPtr ptr)

createProgram :: MonadIO m => String -> String -> m Program
createProgram vsrc fsrc = do
    program <- glCreateProgram
    glAttachShader program =<< createShader GL_VERTEX_SHADER   vsrc
    glAttachShader program =<< createShader GL_FRAGMENT_SHADER fsrc
    glLinkProgram program

    printProgramErrors program

    -- glDeleteShader vshader
    -- glDeleteShader fshader

    return program

printProgramErrors :: MonadIO m => Program -> m ()
printProgramErrors program = do
    result <- liftIO $ alloca $ \ptr ->
        glGetProgramiv program GL_LINK_STATUS ptr >> peek ptr
    when (result == GL_FALSE) $ do
        len <- fmap fromIntegral $ liftIO $ alloca $ \ptr ->
            glGetProgramiv program GL_INFO_LOG_LENGTH ptr >> peek ptr
        errors <- liftIO $ allocaArray len $ \ptr -> do
            glGetProgramInfoLog program (fromIntegral len) nullPtr ptr
            peekCString ptr
        liftIO $ putStrLn errors

createShader :: MonadIO m => GLenum -> String -> m Shader
createShader st src = do
    shader <- glCreateShader st -- FRAGMENT_SHADER / VERTEX_SHADER
    shaderSource shader src
    glCompileShader shader

    printShaderErrors shader

    -- when not getShaderParameter shader COMPILE_STATUS print error
    return shader

printShaderErrors :: MonadIO m => Shader -> m ()
printShaderErrors shader = do
    result <- liftIO $ with GL_FALSE $ \ptr ->
        glGetShaderiv shader GL_COMPILE_STATUS ptr >> peek ptr
    when (result == GL_FALSE) $ do
        len <- fmap fromIntegral $ liftIO $ alloca $ \ptr ->
            glGetShaderiv shader GL_INFO_LOG_LENGTH ptr >> peek ptr
        errors <- liftIO $ allocaArray len $ \ptr -> do
            glGetShaderInfoLog shader (fromIntegral len) nullPtr ptr
            peekCString ptr
        liftIO $ putStrLn errors

shaderSource :: MonadIO m => Shader -> String -> m ()
shaderSource shader src =
    liftIO $ withCString src $ \ptr -> with ptr $ \pptr -> do
        glShaderSource shader 1 pptr nullPtr

setUniformMatrix :: MonadIO m => Program -> String -> Mat4 -> m ()
setUniformMatrix program name mat = do
    loc <- getUniformLocation program name
    withArrayGL (mat4ToList mat) $ \_ ptr ->
        glUniformMatrix4fv loc 1 GL_TRUE ptr

mat4ToList :: Mat4 -> [Float]
mat4ToList = map realToFrac . concatMap toList . toList

class Uniform a where
    setUniform :: MonadIO m => Program -> String -> a -> m ()

instance Uniform Mat4 where
    setUniform = setUniformMatrix

instance Uniform (V4 Double) where
    setUniform program name vec = do
        loc <- getUniformLocation program name
        let V4 x y z w = fmap realToFrac vec
        glUniform4f loc x y z w

instance Uniform (V4 Float) where
    setUniform program name vec = do
        loc <- getUniformLocation program name
        let V4 x y z w = vec
        glUniform4f loc x y z w

instance Uniform Int where
    setUniform program name x = do
        loc <- getUniformLocation program name
        glUniform1i loc (fromIntegral x)

instance Uniform Bool where
    setUniform program name x = do
        loc <- getUniformLocation program name
        glUniform1i loc (if x then GL_TRUE else GL_FALSE)

getUniformLocation :: MonadIO m => Program -> String -> m UniformLocation
getUniformLocation program name =
    liftIO $ withCString name $ glGetUniformLocation program

getAttribLocation :: MonadIO m => Program -> String -> m AttribLocation
getAttribLocation program name =
    liftIO $ withCString name $ glGetAttribLocation program

genObject :: MonadIO m => (GLsizei -> Ptr GLuint -> IO ()) -> m GLuint
genObject genFunc = liftIO $ allocaArray n $ \buf -> do
    genFunc (fromIntegral n) buf
    Unsafe.unsafeHead <$> peekArray n buf
    where n = 1 :: Int

delObject :: MonadIO m => (GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> m ()
delObject delFunc delId = liftIO $ withArrayLen [delId] $ \len buf -> do
    delFunc (fromIntegral len) buf

mkTransformationMatXY :: Num a => M22 a -> V2 a -> M44 a
mkTransformationMatXY m (V2 x y) = identity & _m22 .~ m
    & _x._w .~ x
    & _y._w .~ y

mkMatHomo2 :: Num a => Transformation V2 a -> M44 a
mkMatHomo2 t = mkTransformationMatXY (mkMat t) (transl t)

mkModelVecXY :: Num a => Transformation V2 a -> V2 (V3 a)
mkModelVecXY t = V2 (mk3 rx tx) (mk3 ry ty)
    where
    V2 rx ry = mkMat t
    V2 tx ty = transl t
    mk3 (V2 a b) c = V3 a b c

--------------------------------------------------------------------------------

bindTexture0 :: MonadIO m => Program -> String -> Texture -> m ()
bindTexture0 program name tex = do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D tex
    loc <- getUniformLocation program name
    glUniform1i loc 0

bindTextureN :: MonadIO m => Int -> Program -> String -> Texture -> m ()
bindTextureN itex program name tex = do
    glActiveTexture (GL_TEXTURE0 + fromIntegral itex)
    glBindTexture GL_TEXTURE_2D tex
    loc <- getUniformLocation program name
    glUniform1i loc (fromIntegral itex)

createTexture2dWith :: MonadIO m => m () -> m Texture
createTexture2dWith setTextureAction = do
    tex <- glCreateTexture
    glBindTexture GL_TEXTURE_2D tex

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR

    setTextureAction

    glBindTexture GL_TEXTURE_2D noTexture
    return tex

createTexture2dPtr :: MonadIO m => V2 Int -> ForeignPtr Word8 -> m Texture
createTexture2dPtr (V2 w h) fptr = createTexture2dWith $ do
    liftIO $ withForeignPtr fptr $ \ptr -> glTexImage2D
        GL_TEXTURE_2D 0 GL_R8 (fromIntegral w) (fromIntegral h) 0
        GL_RED GL_UNSIGNED_BYTE (castPtr ptr)

setTexImage2D :: MonadIO m => Image PixelRGBA8 -> m ()
setTexImage2D (Image w h bytes) =
    liftIO $ unsafeWith bytes $ \ptr -> texImage2D_RGBA w h (castPtr ptr)

texImage2D_RGBA :: (MonadIO m, Integral a) => a -> a -> Ptr () -> m ()
texImage2D_RGBA w h ptr = glTexImage2D
    GL_TEXTURE_2D 0 GL_RGBA (fromIntegral w) (fromIntegral h) 0
    GL_RGBA GL_UNSIGNED_BYTE ptr

createTexture2d :: MonadIO m => DynamicImage -> m Texture
createTexture2d = createTexture2dWith . setTexImage2D . convertRGBA8
    -- let Image w h bytes = convertRGBA8 dyn

--------------------------------------------------------------------------------

createTextureBufferFrom :: MonadIO m => DynamicImage -> m TextureBuffer
createTextureBufferFrom dyn = do
    let Image w h bytes = convertRGBA8 dyn
    liftIO $ unsafeWith bytes $ \ptr ->
        createTextureBuffer (fromIntegral w) (fromIntegral h) (castPtr ptr)

createTextureBuffer :: MonadIO m
    => GLint -> GLint -> Ptr () -> m TextureBuffer
createTextureBuffer w h ptr = do
    fbuf <- glCreateFramebuffer
    glBindFramebuffer GL_FRAMEBUFFER fbuf
    -- setFramebufferSize fbuf w h

    -- Create Target Texture
    tex <- glCreateTexture
    glBindTexture   GL_TEXTURE_2D tex
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    texImage2D_RGBA w h ptr

    glFramebufferTexture2D
        GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0

    glBindTexture     GL_TEXTURE_2D  noTexture
    glBindFramebuffer GL_FRAMEBUFFER noFramebuffer

    return $ TextureBuffer
        { field_framebuffer = fbuf
        , field_texture     = tex
        , field_width       = w
        , field_height      = h
        }

withTextureBuffer :: MonadIO m => TextureBuffer -> m a -> m a
withTextureBuffer buf action = do
    glDisable GL_DEPTH_TEST
    let (w, h) = (buf^.width, buf^.height)
    glViewport 0 0 w h
    glBindFramebuffer GL_FRAMEBUFFER (buf^.framebuffer)
    ret <- action
    glBindFramebuffer GL_FRAMEBUFFER noFramebuffer
    return ret

doneTextureBuffer :: MonadIO m => TextureBuffer -> m ()
doneTextureBuffer b = do
    delObject glDeleteTextures     $ b^.texture
    delObject glDeleteFramebuffers $ b^.framebuffer

--------------------------------------------------------------------------------

{-
loadImage
    :: MonadIO m
    => [FilePath]
    -> Chan (FilePath, Maybe DynamicImage)
    -> FilePath -> m ()
loadImage sdirs chan path = liftIO $ do
    void $ forkIO $ do
        mpath <- findFile sdirs path
        case mpath of
            Nothing      -> writeChan chan (path, Nothing)
            Just thePath -> do
                eimg <- force <$> readImage thePath
                case eimg of
                    Left  _   -> writeChan chan (path, Nothing)
                    Right img -> writeChan chan (path, Just img)
-}

loadImageSync :: MonadIO m => FilePath -> m (Maybe DynamicImage)
loadImageSync path = do
    eimg <- force <$> liftIO (readImage path)
    case eimg of
        Left  err -> print err >> return Nothing
        Right img -> return (Just img)
    -- return $ either (const Nothing) Just eimg

glGetInteger :: MonadIO m => GLenum -> m GLint
glGetInteger pname = liftIO $ alloca $ \ptr ->
    glGetIntegerv pname ptr >> peek ptr

