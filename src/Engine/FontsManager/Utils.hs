module Engine.FontsManager.Utils where

import Delude
import qualified Prelude as Unsafe

import Foreign hiding (void)
import Foreign.C.String (withCString)
import Data.ByteString (packCStringLen)
import qualified Data.ByteString as BS
import Data.Vector.Storable (Vector, generate)

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.Bitmap (FT_Bitmap)
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as Bitmap

-- import Codec.Picture.Bitmap
import Codec.Picture

import Engine.FontsManager.FreeTypeError
import Engine.FontsManager.Types

--------------------------------------------------------------------------------

-- | Initialize freetype2 context.
initFreeType :: MonadIO m => m (Either FreeTypeError FT_Library)
initFreeType = allocaFreeType ft_Init_FreeType

-- | Free freetype2 context.
doneFreeType :: MonadIO m => FT_Library -> m ()
doneFreeType = void . liftIO . ft_Done_FreeType

-- | Load and create new font face from a given path.
newFontFace :: MonadIO m
    => FT_Library -- ^ freetype2 context
    -> String     -- ^ font location
    -> Word64     -- ^ face index (see freetype2 documentation for FT_Open_Face)
    -> m (Either FreeTypeError FT_Face)
newFontFace lib path idx = allocaFreeType $ \ptr ->
    withCString path $ \s -> ft_New_Face lib s (fromIntegral idx) ptr

-- | Free face.
doneFontFace :: MonadIO m => FT_Face -> m ()
doneFontFace = void . liftIO . ft_Done_Face

--------------------------------------------------------------------------------

glyphToImage :: MonadIO m => FT_Bitmap -> m (Maybe DynamicImage)
glyphToImage bitmap = case Bitmap.pixel_mode bitmap of
    PixelMode_Gray -> do
        let (h, w) = (Bitmap.rows bitmap, Bitmap.width bitmap)
        bs <- liftIO $ packCStringLen (Bitmap.buffer bitmap, fromIntegral $ w*h)
        return . Just . ImageRGBA8 . grayToColor $ mkImage8 w h bs
    _ -> return Nothing

grayToColor :: Image Pixel8 -> Image PixelRGBA8
grayToColor = pixelMap $ \c -> PixelRGBA8 1 1 1 c

-- | O(n) Convert ByteString to Vector Word8
bsToV :: ByteString -> Vector Word8
bsToV bs = generate (BS.length bs) (BS.index bs)

mkImage8 :: Integral a => a -> a -> ByteString -> Image Word8
mkImage8 w h bs = Image (fromIntegral w) (fromIntegral h) (bsToV bs)

--------------------------------------------------------------------------------

prettyFreeTypeError :: CallStack -> FreeTypeError -> String
prettyFreeTypeError cs err = srcLoc <> toString errorMsg
    where
    errorMsg = freeTypeError_toErrorMessage err
    srcLoc   = callStackFileLine cs

logFreeType :: (HasCallStack, MonadIO m)
    => m (Either FreeTypeError a) -> m (Maybe a)
logFreeType mErrVal = mErrVal >>= \case
    Right val -> return $ Just val
    Left  err -> do
        putStrLn $ prettyFreeTypeError callStack err
        return Nothing

allocaFreeType :: (MonadIO m, Storable a)
    => (Ptr a -> IO FT_Error) -> m (Either FreeTypeError a)
allocaFreeType ini = liftIO $ alloca $ \ptr -> do
    err <- ini ptr
    if err == 0
    then Right <$> peek ptr
    else return $ Left $ freeTypeError_fromErrorCode err

assertFreeTypeEither :: (HasCallStack, MonadIO m)
    => m (Either FreeTypeError a) -> m a
assertFreeTypeEither mErrVal = mErrVal >>= \case
    Right val -> return val
    Left  err -> Unsafe.error $ prettyFreeTypeError callStack err

assertFreeType :: (HasCallStack, MonadIO m) => IO FT_Error -> IO a -> m a
assertFreeType merr mval = do
    err <- liftIO merr
    if err == 0
    then liftIO $ mval
    else Unsafe.error $ prettyFreeTypeError callStack
                      $ freeTypeError_fromErrorCode err

assertFreeType_ :: (HasCallStack, MonadIO m) => IO FT_Error -> m ()
assertFreeType_ merr = assertFreeType merr (return ())

