{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
module Engine.Layout.Types (module Engine.Layout.Types) where

import Delude
import qualified Data.Colour as Color
import Engine.FontsManager.Types
import Engine.Graphics.Types

import Engine.Common.Types as Engine.Layout.Types

--------------------------------------------------------------------------------

data BorderDesc = BorderDesc
   { borderDesc_width :: AbsoluteSize
   , borderDesc_color :: AlphaColor
   }
makeFieldsCustom ''BorderDesc
instance Default BorderDesc where
    def = BorderDesc
        { borderDesc_width = 0
        , borderDesc_color = Color.opaque Color.black
        }

data BoxEdges a = BoxEdges
   { boxEdges_top    :: a
   , boxEdges_left   :: a
   , boxEdges_bottom :: a
   , boxEdges_right  :: a
   } deriving (Functor, Foldable, Traversable)
makeFieldsCustom ''BoxEdges
instance Default a => Default (BoxEdges a) where
    def = BoxEdges
        { boxEdges_top    = def
        , boxEdges_left   = def
        , boxEdges_bottom = def
        , boxEdges_right  = def
        }

instance Each (BoxEdges a) (BoxEdges b) a b where
    each f (BoxEdges a b c d) = BoxEdges <$> f a <*> f b <*> f c <*> f d

traverseVertical :: Traversal' (BoxEdges a) a
traverseVertical f (BoxEdges a b c d) = BoxEdges
    <$> pure a <*> f b <*> pure c <*> f d

traverseHorizontal :: Traversal' (BoxEdges a) a
traverseHorizontal f (BoxEdges a b c d) = BoxEdges
    <$> f a <*> pure b <*> f c <*> pure d

--------------------------------------------------------------------------------

data BoxDesc = BoxDesc
   { boxDesc_boxAlign :: BoxAlign
   , boxDesc_padding  :: BoxEdges AbsoluteSize
   , boxDesc_size     :: Size Sizing
   , boxDesc_color    :: AlphaColor
   , boxDesc_border   :: BoxEdges BorderDesc
   }
makeFieldsCustom ''BoxDesc
instance Default BoxDesc where
    def = BoxDesc
        { boxDesc_boxAlign = Center
        , boxDesc_padding  = def
        , boxDesc_size     = Size (1 @@ fill) (1 @@ fill)
        , boxDesc_color    = Color.transparent
        , boxDesc_border   = def
        }

data SimpleBoxOpts = SimpleBoxOpts
   { simpleBoxOpts_size   :: Size AbsoluteSize
   , simpleBoxOpts_color  :: AlphaColor
   , simpleBoxOpts_border :: BoxEdges BorderDesc
   }
makeFieldsCustom ''SimpleBoxOpts
instance Default SimpleBoxOpts where
    def = SimpleBoxOpts
        { simpleBoxOpts_size   = def
        , simpleBoxOpts_color  = Color.transparent
        , simpleBoxOpts_border = def
        }

data LineupDirection
   = LineupDirection_Vertical
   | LineupDirection_Horizontal

instance HasPatternVertical LineupDirection where
    _PatternVertical = LineupDirection_Vertical
instance HasPatternHorizontal LineupDirection where
    _PatternHorizontal = LineupDirection_Horizontal

data LineupJustify
   = LineupJustify_Start
   | LineupJustify_End
   | LineupJustify_Center
   | LineupJustify_SpaceBetween

instance HasPatternStart LineupJustify where
    _PatternStart = LineupJustify_Start
instance HasPatternEnd LineupJustify where
    _PatternEnd = LineupJustify_End
instance HasPatternCenter LineupJustify where
    _PatternCenter = LineupJustify_Center
instance HasPatternSpaceBetween LineupJustify where
    _PatternSpaceBetween = LineupJustify_SpaceBetween

data LineupDesc = LineupDesc
   { lineupDesc_direction :: LineupDirection
   , lineupDesc_justify   :: LineupJustify
   }
makeFieldsCustom ''LineupDesc
instance Default LineupDesc where
    def = LineupDesc
        { lineupDesc_direction = LineupDirection_Vertical
        , lineupDesc_justify   = LineupJustify_Start
        }

data TextAlign
   = TextAlign_Left
   | TextAlign_Right
   | TextAlign_Center
   | TextAlign_Justify

data TextDesc = TextDesc
   { textDesc_boxAlign      :: BoxAlign
   , textDesc_textAlign     :: TextAlign
   , textDesc_minLineHeight :: AbsoluteSize
   }
makeFieldsCustom ''TextDesc
instance Default TextDesc where
    def = TextDesc
        { textDesc_boxAlign      = Center
        , textDesc_textAlign     = TextAlign_Left
        -- , textDesc_textAlign     = TextAlign_Center
        , textDesc_minLineHeight = 0
        }

type FontStyle    = FontStyleF [FontFamilyName]
type FontStyleRes = FontStyleF FontHierarchy

data RichText
   = RichText_Span  FontStyle Text
   | RichText_Image Img

data Layout
   = Layout_Box    BoxDesc    [Layout]
   | Layout_Lineup LineupDesc [Layout]
   | Layout_Text   TextDesc   [RichText]
   | Layout_Empty

layoutBox :: BoxDesc -> [Layout] -> Layout
layoutBox = Layout_Box

layoutLineup :: LineupDesc -> [Layout] -> Layout
layoutLineup = Layout_Lineup

layoutText :: TextDesc -> [RichText] -> Layout
layoutText = Layout_Text

layoutEmpty :: Layout
layoutEmpty = Layout_Empty

verticalLineup :: [Layout] -> Layout
verticalLineup = Layout_Lineup (def & direction .~ Vertical)

horizontalLineup :: [Layout] -> Layout
horizontalLineup = Layout_Lineup (def & direction .~ Horizontal)

--------------------------------------------------------------------------------

data FontStyleF a = FontStyle
   { fontStyle_fonts         :: a -- [FontFamilyName] -- FontHierarchy
   , fontStyle_fontSize      :: FontSize
   , fontStyle_color         :: AlphaColor
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

{-
data RichTextTree a
   = RichTextTree_Node FontStyle [RichTextTree a]
   | RichTextTree_Leaf a

data RichTextValue
   = RichTextValue_Text  Text
   | RichTextValue_Image Image

-- type RichText = RichTextTree RichTextValue
pattern RichText_Span fs t
      = RichTextTree_Node fs [RichTextTree_Leaf $ RichTextValue_Text t]
pattern RichText_Image img = RichTextTree_Leaf $ RichTextValue_Image img
-}

data TextLayout = TextLayout
   { textLayout_textAlign     :: TextAlign
   , textLayout_size          :: Size AbsoluteSize
   , textLayout_minLineHeight :: AbsoluteSize
   , textLayout_content       :: [RichText]
   }
makeFieldsCustom ''TextLayout
instance Default TextLayout where
    def = TextLayout
        { textLayout_textAlign     = TextAlign_Left
        , textLayout_size          = def
        , textLayout_minLineHeight = 0
        , textLayout_content       = []
        }

--------------------------------------------------------------------------------

{-
newtype SizeInPixels = SPix Int

partsPerPixel :: Int
partsPerPixel = 64

--}
