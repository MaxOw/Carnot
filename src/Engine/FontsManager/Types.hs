{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
{-# Language PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
module Engine.FontsManager.Types where

import Delude
import qualified Data.Colour as Color

import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face (FT_Face)
import Graphics.Rendering.FreeType.Internal.Library (FT_Library)

import Engine.Backend.Types (TextureBuffer)

--------------------------------------------------------------------------------

pattern PixelMode_None  = 0 -- ft_PIXEL_MODE_NONE
pattern PixelMode_Mono  = 1 -- ft_PIXEL_MODE_MONO
pattern PixelMode_Gray  = 2 -- ft_PIXEL_MODE_GRAY
pattern PixelMode_Gray2 = 3 -- ft_PIXEL_MODE_GRAY2
pattern PixelMode_Gray4 = 4 -- ft_PIXEL_MODE_GRAY4
pattern PixelMode_LCD   = 5 -- ft_PIXEL_MODE_LCD
pattern PixelMode_LCDV  = 6 -- ft_PIXEL_MODE_LCDV
pattern PixelMode_BGRA  = 7 -- ft_PIXEL_MODE_BGRA

--------------------------------------------------------------------------------

type DPI = Int

type Advance  = Int

-- newtype GlyphIndex = GlyphIndex FT_UInt
type GlyphIndex = FT_UInt
type GlyphFlags = FT_Int32

--------------------------------------------------------------------------------

data Glyph = Glyph
   { glyph_buffer  :: TextureBuffer
   , glyph_bearing :: V2 Int
   , glyph_size    :: V2 Int
   , glyph_advance :: Int
   }
makeFieldsCustom ''Glyph

--------------------------------------------------------------------------------

type FontName = Text
type FontSize = Int
data CharWithSize = CharWithSize Char FontSize deriving (Eq, Generic)
instance Hashable CharWithSize

data Font = Font
   { font_ftFace    :: FT_Face
   , font_deviceDPI :: DPI
   , font_glyphsMap :: TVar (HashMap CharWithSize (Maybe Glyph))
   }
makeFieldsCustom ''Font
instance Eq  Font where (==)    a b = (==)    (a^.ftFace) (b^.ftFace)
instance Ord Font where compare a b = compare (a^.ftFace) (b^.ftFace)

type FontFamilyName = Text

data FontFamilyWith a = FontFamily
   { fontFamily_fontBase       :: a
   , fontFamily_fontItalic     :: Maybe a
   , fontFamily_fontBold       :: Maybe a
   , fontFamily_fontBoldItalic :: Maybe a
   }
makeFieldsCustom ''FontFamilyWith
instance Default a => Default (FontFamilyWith a) where
  def = FontFamily
    { fontFamily_fontBase       = def
    , fontFamily_fontItalic     = def
    , fontFamily_fontBold       = def
    , fontFamily_fontBoldItalic = def
    }

type FontFamily     = FontFamilyWith Font
type FontFamilyDesc = FontFamilyWith FilePath

data GlyphKey = GlyphKey
   { glyphKey_keyChar     :: Char
   , glyphKey_keyFontSize :: FontSize
   , glyphKey_keyBold     :: Bool
   , glyphKey_keyItalic   :: Bool
   } deriving (Eq, Generic)
makeFieldsCustom ''GlyphKey
instance Hashable GlyphKey

data FontHierarchy = FontHierarchy
   { fontHierarchy_hierarchy :: [FontFamily]
   , fontHierarchy_glyphsMap :: TVar (HashMap GlyphKey (Maybe Glyph))
   }
makeFieldsCustom ''FontHierarchy

data FontMetrics = FontMetrics
   { fontMetrics_verticalOffset  :: Int
   , fontMetrics_lineHeight      :: Int
   , fontMetrics_minSpaceAdvance :: Int
   }
makeFieldsCustom ''FontMetrics

--------------------------------------------------------------------------------

data FontsManager = FontsManager
   { fontsManager_ftLib          :: FT_Library
   , fontsManager_deviceDPI      :: DPI
   , fontsManager_fontsMap       :: TVar (HashMap FontName Font)
   , fontsManager_familiesMap    :: TVar (HashMap FontFamilyName FontFamily)
   , fontsManager_hierarchiesMap :: TMVar (HashMap [FontFamilyName] FontHierarchy)
   }
makeFieldsCustom ''FontsManager

--------------------------------------------------------------------------------

data FontStyleF a = FontStyle
   { fontStyle_fonts         :: a -- [FontFamilyName] -- FontHierarchy
   , fontStyle_fontSize      :: FontSize
   , fontStyle_color         :: Color.AlphaColour Float
   , fontStyle_bold          :: Bool
   , fontStyle_italic        :: Bool
   , fontStyle_underscore    :: Bool
   , fontStyle_strikethrough :: Bool
   }
makeFieldsCustom ''FontStyleF

-- makeFontStyle :: FontHierarchy -> FontSize -> FontStyle
makeFontStyle :: a -> FontSize -> FontStyleF a
makeFontStyle f fs = FontStyle
   { fontStyle_fonts         = f
   , fontStyle_fontSize      = fs
   , fontStyle_color         = Color.opaque Color.black
   , fontStyle_bold          = False
   , fontStyle_italic        = False
   , fontStyle_underscore    = False
   , fontStyle_strikethrough = False
   }

type FontStyle    = FontStyleF [FontFamilyName]
type FontStyleRes = FontStyleF FontHierarchy

instance Default a => Default (FontStyleF a) where
    def = makeFontStyle def 10

--------------------------------------------------------------------------------

