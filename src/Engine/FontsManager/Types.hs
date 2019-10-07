{-# Language StrictData #-}
{-# Language PatternSynonyms #-}
{-# Language StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
module Engine.FontsManager.Types where

import Delude
import qualified Data.Colour as Color
import Data.Hashable

import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face (FT_Face)
import Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import Data.Colour.SRGB

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
   { field_buffer  :: TextureBuffer
   , field_bearing :: V2 Int
   , field_size    :: V2 Int
   , field_advance :: Int
   } deriving (Generic)
instance HasSize Glyph (V2 Int)

--------------------------------------------------------------------------------

type FontName = Text
type FontSize = Int
data CharWithSize = CharWithSize Char FontSize deriving (Eq, Generic)
instance Hashable CharWithSize

data Font = Font
   { field_ftFace    :: FT_Face
   , field_deviceDPI :: DPI
   , field_glyphsMap :: TVar (HashMap CharWithSize (Maybe Glyph))
   } deriving (Generic)
instance Eq  Font where (==)    a b = (==)    (a^.ftFace) (b^.ftFace)
instance Ord Font where compare a b = compare (a^.ftFace) (b^.ftFace)

type FontFamilyName = Text

data FontFamilyWith a = FontFamily
   { field_fontBase       :: a
   , field_fontItalic     :: Maybe a
   , field_fontBold       :: Maybe a
   , field_fontBoldItalic :: Maybe a
   } deriving (Generic)
instance Default a => Default (FontFamilyWith a) where
  def = FontFamily
    { field_fontBase       = def
    , field_fontItalic     = def
    , field_fontBold       = def
    , field_fontBoldItalic = def
    }

type FontFamily     = FontFamilyWith Font
type FontFamilyDesc = FontFamilyWith FilePath

data GlyphKey = GlyphKey
   { field_keyChar     :: Char
   , field_keyFontSize :: FontSize
   , field_keyBold     :: Bool
   , field_keyItalic   :: Bool
   } deriving (Eq, Generic)
instance Hashable GlyphKey

data FontHierarchy = FontHierarchy
   { field_hierarchy :: [FontFamily]
   , field_glyphsMap :: TVar (HashMap GlyphKey (Maybe Glyph))
   } deriving (Generic)

data FontMetrics = FontMetrics
   { field_verticalOffset  :: Int
   , field_lineHeight      :: Int
   , field_minSpaceAdvance :: Int
   } deriving (Generic)

--------------------------------------------------------------------------------

data FontsManager = FontsManager
   { field_ftLib          :: FT_Library
   , field_deviceDPI      :: DPI
   , field_fontsMap       :: TVar (HashMap FontName Font)
   , field_familiesMap    :: TVar (HashMap FontFamilyName FontFamily)
   , field_hierarchiesMap :: TMVar (HashMap [FontFamilyName] FontHierarchy)
   } deriving (Generic)

--------------------------------------------------------------------------------

data FontStyleF a = FontStyle
   { field_fonts         :: a -- [FontFamilyName] -- FontHierarchy
   , field_fontSize      :: FontSize
   , field_color         :: Color.AlphaColour Float
   , field_bold          :: Bool
   , field_italic        :: Bool
   , field_underscore    :: Bool
   , field_strikethrough :: Bool
   } deriving (Eq, Generic)
instance Hashable a => Hashable (FontStyleF a)

{-
    hashWithSalt s (FontStyle a b c d e f g)
        = hashWithSalt s (a, b, fromAlphaColor c, d, e, f, g)
        where
        fromAlphaColor c = (r, g, b, a)
            where
            RGB r g b = toSRGB $ Color.over c Color.black
            a = Color.alphaChannel c
-}

hashFontStyle :: Hashable a => FontStyleF a -> Int
hashFontStyle (FontStyle a b _ d e f g) = hash (a, b, d, e, f, g)

-- makeFontStyle :: FontHierarchy -> FontSize -> FontStyle
makeFontStyle :: a -> FontSize -> FontStyleF a
makeFontStyle f fs = FontStyle
   { field_fonts         = f
   , field_fontSize      = fs
   , field_color         = Color.opaque Color.black
   , field_bold          = False
   , field_italic        = False
   , field_underscore    = False
   , field_strikethrough = False
   }

type FontStyle    = FontStyleF [FontFamilyName]
type FontStyleRes = FontStyleF FontHierarchy

instance Default a => Default (FontStyleF a) where
    def = makeFontStyle def 10

--------------------------------------------------------------------------------

