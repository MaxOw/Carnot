{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
module Engine.FontsManager
    ( module Engine.FontsManager.Types

    , getFontMetrics
    , font_loadGlyph
    , hierarchy_loadGlyph
    , getPrimaryFont

    , initFontsManager
    , managerLoadFont
    , managerLoadFontFamily
    , managerCreateFontHierarchy
    , lookupFont
    )where

import Delude
import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.STM.TMVar

import Foreign hiding (void)
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import qualified Graphics.Rendering.FreeType.Internal.Face as Face
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as GlyphSlot
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics as Metrics
import qualified Graphics.Rendering.FreeType.Internal.Size as Size
import qualified Graphics.Rendering.FreeType.Internal.SizeMetrics as SizeMetrics
-- import qualified Graphics.Rendering.FreeType.Internal.BBox as BBox

import Engine.FontsManager.Types
-- import Engine.FontsManager.FreeTypeError
import Engine.FontsManager.Utils
import Engine.Graphics.Utils

--------------------------------------------------------------------------------

getFontMetrics :: MonadIO m => Font -> FontSize -> m FontMetrics
getFontMetrics font fsize = do
    mSpaceAdv <- fmap (view advance) <$> font_loadGlyph font fsize ' '
    faceSize <- liftIO $ peek $ Face.size (font^.ftFace)
    -- bbox <- liftIO $ peek $ Face.bbox (font^.ftFace)
    sizeMetrics <- liftIO $ peek $ Size.metrics faceSize
    -- let yMax = fromIntegral $ BBox.yMax bbox
    let faceHeight = fromIntegral $ SizeMetrics.height sizeMetrics
    let asc = fromIntegral $ SizeMetrics.ascender sizeMetrics
    -- let desc = fromIntegral . abs $ SizeMetrics.descender sizeMetrics
    let maxAdv = fromIntegral $ SizeMetrics.max_advance sizeMetrics
    return $ FontMetrics
        { fontMetrics_verticalOffset  = asc -- yMax
        , fontMetrics_lineHeight      = faceHeight
        , fontMetrics_minSpaceAdvance = fromMaybe maxAdv mSpaceAdv
        }

-- We should be probably also caching failures here in glyphsMap, that is to
-- change type of glyphsMap from:
-- font_glyphsMap :: TVar (HashMap CharWithSize Glyph)
-- to:
-- font_glyphsMap :: TVar (HashMap CharWithSize (Maybe Glyph))
font_loadGlyph :: MonadIO m => Font -> FontSize -> Char -> m (Maybe Glyph)
font_loadGlyph font fsize char =
    lookupInsertM (font^.glyphsMap) key newV relV
    where
    key  = CharWithSize char fsize
    relV = doneTextureBuffer . view buffer
    newV = do
        let dpi  = fromIntegral $ font^.deviceDPI
        let fs   = fromIntegral fsize * 64
        let face = font^.ftFace

        -- TODO: Fix this up...
        assertFreeType_ $ ft_Set_Char_Size face 0 fs 0 dpi
        gindex <- liftIO $ ft_Get_Char_Index face (fromIntegral $ ord char)
        if gindex == 0 then return Nothing else do
            assertFreeType_ $ ft_Load_Glyph face gindex 0

            glyphSlot   <- liftIO $ peek $ Face.glyph face
            glyphFormat <- liftIO $ peek $ GlyphSlot.format glyphSlot

            when (glyphFormat /= ft_GLYPH_FORMAT_BITMAP) $ do
                let render_mode = ft_RENDER_MODE_NORMAL
                assertFreeType_ $ ft_Render_Glyph glyphSlot render_mode

            glyphBitmap <- liftIO $ peek $ GlyphSlot.bitmap glyphSlot
            -- let pixelMode = Bitmap.pixel_mode glyphBitmap

            ms <- liftIO $ peek $ GlyphSlot.metrics glyphSlot

            let bear = V2 (Metrics.horiBearingX ms) (Metrics.horiBearingY ms)
            let gsiz = V2 (Metrics.width ms) (Metrics.height ms)
            let adv  = fromIntegral $ Metrics.horiAdvance ms

            mImg <- glyphToImage glyphBitmap
            caseJust mImg $ \img -> do
                buff <- createTextureBufferFrom img
                return $ Just $ Glyph
                    { glyph_buffer  = buff
                    , glyph_bearing = over each fromIntegral bear
                    , glyph_size    = over each fromIntegral gsiz
                    , glyph_advance = adv
                    }

hierarchy_loadGlyph :: MonadIO m => FontHierarchy -> GlyphKey -> m (Maybe Glyph)
hierarchy_loadGlyph h key = lookupInsertM (h^.glyphsMap) key newV relV
    where
    newV = getFirstM $ map getFromFamily $ h^.hierarchy
    relV = const $ return ()

    load f = font_loadGlyph f (key^.keyFontSize) (key^.keyChar)

    getFromFamily f
        | key^.keyBold && key^.keyItalic = caseJust (f^.fontBoldItalic) load
        | key^.keyBold                   = caseJust (f^.fontBold) load
        | key^.keyItalic                 = caseJust (f^.fontItalic) load
        | otherwise                      = load $ f^.fontBase

getFirstM :: Monad m => [m (Maybe a)] -> m (Maybe a)
getFirstM [] = return Nothing
getFirstM (l:ls) = l >>= \case
    Nothing -> getFirstM ls
    justVal -> return justVal

--------------------------------------------------------------------------------

initFontsManager :: MonadIO m => Maybe DPI -> m FontsManager
initFontsManager mDPI = do
    lib <- assertFreeTypeEither initFreeType
    fontsMapVar <- newTVarIO mempty
    familiesMapVar <- newTVarIO mempty
    hierarchiesMapVar <- atomically $ newTMVar mempty
    return $ FontsManager
        { fontsManager_ftLib          = lib
        , fontsManager_deviceDPI      = fromMaybe 72 mDPI
        , fontsManager_fontsMap       = fontsMapVar
        , fontsManager_familiesMap    = familiesMapVar
        , fontsManager_hierarchiesMap = hierarchiesMapVar
        }

managerLoadFont :: MonadIO m
    => FontsManager -> FontName -> FilePath -> m (Maybe Font)
managerLoadFont manager name path =
    lookupInsert (manager^.fontsMap) name newV relV
    where
    relV = releaseFont
    newV = do
        mFace <- logFreeType $ newFontFace (manager^.ftLib) path 0
        mapM makeFont mFace

    makeFont face = do
        fontGlyphsMap <- newTVarIO mempty
        return $ Font
            { font_ftFace    = face
            , font_deviceDPI = manager^.deviceDPI
            , font_glyphsMap = fontGlyphsMap
            }

releaseFont :: MonadIO m => Font -> m ()
releaseFont = doneFontFace . view ftFace

managerLoadFontFamily :: MonadIO m
    => FontsManager -> FontFamilyName -> FontFamilyDesc -> m (Maybe FontFamily)
managerLoadFontFamily m name desc =
    lookupInsert (m^.familiesMap) name newV relV
    where
    relV = releaseFontFamily
    newV = caseJustM loadBase $ \base -> fmap Just $ FontFamily
        <$> pure base
        <*> loadM (name<>"-Italic")      (desc^.fontItalic)
        <*> loadM (name<>"-Bold")        (desc^.fontBold)
        <*> loadM (name<>"-Bold-Italic") (desc^.fontBoldItalic)

    loadBase = managerLoadFont m name $ desc^.fontBase

    loadM _ Nothing  = return Nothing
    loadM n (Just p) = managerLoadFont m n p

releaseFontFamily :: MonadIO m => FontFamily -> m ()
releaseFontFamily d = do
    releaseFont (d^.fontBase)
    mapM_ releaseFont (d^.fontItalic)
    mapM_ releaseFont (d^.fontBold)
    mapM_ releaseFont (d^.fontBoldItalic)

managerCreateFontHierarchy :: MonadIO m
    => FontsManager -> [FontFamilyName] -> m FontHierarchy
managerCreateFontHierarchy m names = do
    let var = view hierarchiesMap m
    hmap <- atomically $ takeTMVar var
    case HashMap.lookup names hmap of
        Just hier -> do
            atomically $ putTMVar var hmap
            return hier
        Nothing -> do
            newHier <- createHierarchy
            atomically $ putTMVar var $ HashMap.insert names newHier hmap
            return newHier
    where
    createHierarchy = FontHierarchy
        <$> fmap catMaybes (mapM (lookupFontFamily m) names)
        <*> newTVarIO mempty

getPrimaryFont :: FontHierarchy -> Bool -> Bool -> Maybe Font
getPrimaryFont h isBold isItalic
    | isBold && isItalic = h^?hierarchy._head.fontBoldItalic & join
    | isBold             = h^?hierarchy._head.fontBold       & join
    | isItalic           = h^?hierarchy._head.fontItalic     & join
    | otherwise          = h^?hierarchy._head.fontBase

lookupFontFamily :: MonadIO m
    => FontsManager -> FontFamilyName -> m (Maybe FontFamily)
lookupFontFamily m n = HashMap.lookup n <$> readRecordTVar m familiesMap

lookupFont :: MonadIO m => FontsManager -> FontName -> m (Maybe Font)
lookupFont m name = HashMap.lookup name <$> readRecordTVar m fontsMap

-- This is so complicated to account for a situation when some other
-- thread has added value with this key in a meantime. In that case we
-- return value that the other thread has added and release this resource.
lookupInsert :: (MonadIO m, Eq k, Hashable k)
    => TVar (HashMap k v) -- ^ TVar with HashMap for caching
    -> k                  -- ^ Resource key
    -> m (Maybe v)        -- ^ Resource creation function
    -> (v -> m ())        -- ^ Resource release function
    -> m (Maybe v)
lookupInsert var key createValue releaseValue = lookupValue >>= \case
    Just vl -> return $ Just vl
    Nothing -> caseJustM createValue $ \newValue -> do
        eValue <- properAdd newValue
        whenLeft_ eValue $ \_ -> releaseValue newValue
        return $ Just $ either id id eValue
    where
    lookupValue = HashMap.lookup key <$> readTVarIO var
    properAdd newValue = atomically $ do
        m <- readTVar var
        case HashMap.lookup key m of
            Nothing -> do
                writeTVar var $ HashMap.insert key newValue m
                return $ Right newValue
            Just oldFont -> do
                return $ Left oldFont

-- This version also caches failures
lookupInsertM :: (MonadIO m, Eq k, Hashable k)
    => TVar (HashMap k (Maybe v)) -- ^ TVar with HashMap for caching
    -> k                          -- ^ Resource key
    -> m (Maybe v)                -- ^ Resource creation function
    -> (v -> m ())                -- ^ Resource release function
    -> m (Maybe v)
lookupInsertM var key createValue releaseValue = lookupValue >>= \case
    Just vl -> return vl
    Nothing -> createValue >>= \newValue -> do
        eValue <- properAdd newValue
        whenLeft_ eValue $ \_ -> mapM_ releaseValue newValue
        return $ either id id eValue
    where
    lookupValue = HashMap.lookup key <$> readTVarIO var
    properAdd newValue = atomically $ do
        m <- readTVar var
        case HashMap.lookup key m of
            Nothing -> do
                writeTVar var $ HashMap.insert key newValue m
                return $ Right newValue
            Just oldFont -> do
                return $ Left oldFont

