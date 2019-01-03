{-# Language TemplateHaskell #-}
{-# Language StrictData #-}
{-# Language TypeFamilies #-}
module Engine.Layout.Types (module Engine.Layout.Types) where

import Delude
import Linear
import qualified Data.Colour as Color
import Engine.FontsManager.Types
import Engine.Graphics.Types

import Engine.Common.Types as Engine.Layout.Types

--------------------------------------------------------------------------------

data VerticalAlign
   = Align_Top
   | Align_Middle
   | Align_Bottom

data HorizontalAlign
   = Align_Left
   | Align_Center
   | Align_Right

data BoxAlign = BoxAlign
   { boxAlign_vertical   :: VerticalAlign
   , boxAlign_horizontal :: HorizontalAlign
   }
makeFieldsCustom ''BoxAlign

pattern BoxAlign_TopLeft   = BoxAlign Align_Top Align_Left
pattern BoxAlign_TopCenter = BoxAlign Align_Top Align_Center
pattern BoxAlign_TopRight  = BoxAlign Align_Top Align_Right

pattern BoxAlign_MiddleLeft   = BoxAlign Align_Middle Align_Left
pattern BoxAlign_MiddleCenter = BoxAlign Align_Middle Align_Center
pattern BoxAlign_MiddleRight  = BoxAlign Align_Middle Align_Right

pattern BoxAlign_BottomLeft   = BoxAlign Align_Bottom Align_Left
pattern BoxAlign_BottomCenter = BoxAlign Align_Bottom Align_Center
pattern BoxAlign_BottomRight  = BoxAlign Align_Bottom Align_Right

pattern BoxAlign_Middle = BoxAlign Align_Middle Align_Center
pattern BoxAlign_Center = BoxAlign Align_Middle Align_Center

instance HasPatternTopLeft BoxAlign where
    _PatternTopLeft = BoxAlign_TopLeft
instance HasPatternTopCenter BoxAlign where
    _PatternTopCenter = BoxAlign_TopCenter
instance HasPatternTopRight BoxAlign where
    _PatternTopRight = BoxAlign_TopRight
instance HasPatternMiddleLeft BoxAlign where
    _PatternMiddleLeft = BoxAlign_MiddleLeft
instance HasPatternMiddleCenter BoxAlign where
    _PatternMiddleCenter = BoxAlign_MiddleCenter
instance HasPatternMiddleRight BoxAlign where
    _PatternMiddleRight = BoxAlign_MiddleRight
instance HasPatternBottomLeft BoxAlign where
    _PatternBottomLeft = BoxAlign_BottomLeft
instance HasPatternBottomCenter BoxAlign where
    _PatternBottomCenter = BoxAlign_BottomCenter
instance HasPatternBottomRight BoxAlign where
    _PatternBottomRight = BoxAlign_BottomRight
instance HasPatternCenter BoxAlign where
    _PatternCenter = BoxAlign_Center

{-
instance HasPatternLeft HorizontalAlign where
    _PatternLeft = AlignLeft
instance HasPatternRight HorizontalAlign where
    _PatternRight = AlignRight
-}

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
        , boxDesc_size     = Size (1 @@ cpct) (1 @@ cpct)
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

data BBox a = BBox
   { bbox_offset :: V2 a
   , bbox_size   :: Size a
   }
makeFieldsCustom ''BBox

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

data Image = Image
   { image_texture :: Texture
   , image_size    :: V2 AbsoluteSize
   }
makeFieldsCustom ''Image

data RichText
   = RichText_Span  FontStyle Text
   | RichText_Image Image

data Layout
   = Layout_Box    BoxDesc    [Layout]
   | Layout_Lineup LineupDesc [Layout]
   | Layout_Text   TextDesc   [RichText]

verticalLineup :: [Layout] -> Layout
verticalLineup = Layout_Lineup (def & direction .~ Vertical)

horizontalLineup :: [Layout] -> Layout
horizontalLineup = Layout_Lineup (def & direction .~ Horizontal)

--------------------------------------------------------------------------------

data FontStyle = FontStyle
   { fontStyle_fonts         :: FontHierarchy
   , fontStyle_fontSize      :: FontSize
   , fontStyle_color         :: AlphaColor
   , fontStyle_bold          :: Bool
   , fontStyle_italic        :: Bool
   , fontStyle_underscore    :: Bool
   , fontStyle_strikethrough :: Bool
   }
makeFieldsCustom ''FontStyle

makeFontStyle :: FontHierarchy -> FontSize -> FontStyle
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
