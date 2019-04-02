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
   { field_width :: AbsoluteSize
   , field_color :: AlphaColor
   } deriving (Generic)
instance HasWidth BorderDesc AbsoluteSize
instance Default BorderDesc where
    def = BorderDesc
        { field_width = 0
        , field_color = Color.opaque Color.black
        }

data BoxEdges a = BoxEdges
   { field_top    :: a
   , field_left   :: a
   , field_bottom :: a
   , field_right  :: a
   } deriving (Generic, Functor, Foldable, Traversable)
instance Default a => Default (BoxEdges a) where
    def = BoxEdges
        { field_top    = def
        , field_left   = def
        , field_bottom = def
        , field_right  = def
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
   { field_boxAlign :: BoxAlign
   , field_padding  :: BoxEdges AbsoluteSize
   , field_size     :: Size Sizing
   , field_color    :: AlphaColor
   , field_border   :: BoxEdges BorderDesc
   } deriving (Generic)
instance Default BoxDesc where
    def = BoxDesc
        { field_boxAlign = Center
        , field_padding  = def
        , field_size     = Size (1 @@ fill) (1 @@ fill)
        , field_color    = Color.transparent
        , field_border   = def
        }

data SimpleBoxOpts = SimpleBoxOpts
   { field_size   :: Size AbsoluteSize
   , field_color  :: AlphaColor
   , field_border :: BoxEdges BorderDesc
   } deriving (Generic)
instance Default SimpleBoxOpts where
    def = SimpleBoxOpts
        { field_size   = def
        , field_color  = Color.transparent
        , field_border = def
        }

data LineupDirection
   = LineupDirection_Vertical
   | LineupDirection_Horizontal

instance HasPatternVertical LineupDirection where
    _PatternVertical = LineupDirection_Vertical
instance HasPatternHorizontal LineupDirection where
    _PatternHorizontal = LineupDirection_Horizontal

data LineupJustify
   = LineupJustify_Start
   | LineupJustify_End
   | LineupJustify_Center
   | LineupJustify_SpaceBetween

instance HasPatternStart LineupJustify where
    _PatternStart = LineupJustify_Start
instance HasPatternEnd LineupJustify where
    _PatternEnd = LineupJustify_End
instance HasPatternCenter LineupJustify where
    _PatternCenter = LineupJustify_Center
instance HasPatternSpaceBetween LineupJustify where
    _PatternSpaceBetween = LineupJustify_SpaceBetween

data LineupDesc = LineupDesc
   { field_direction :: LineupDirection
   , field_justify   :: LineupJustify
   } deriving (Generic)
instance Default LineupDesc where
    def = LineupDesc
        { field_direction = LineupDirection_Vertical
        , field_justify   = LineupJustify_Start
        }

data TextAlign
   = TextAlign_Left
   | TextAlign_Right
   | TextAlign_Center
   | TextAlign_Justify

data TextDesc = TextDesc
   { field_boxAlign      :: BoxAlign
   , field_textAlign     :: TextAlign
   , field_minLineHeight :: AbsoluteSize
   } deriving (Generic)
instance Default TextDesc where
    def = TextDesc
        { field_boxAlign      = Center
        , field_textAlign     = TextAlign_Left
        -- , field_textAlign     = TextAlign_Center
        , field_minLineHeight = 0
        }

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
   { field_textAlign     :: TextAlign
   , field_size          :: Size AbsoluteSize
   , field_minLineHeight :: AbsoluteSize
   , field_content       :: [RichText]
   } deriving (Generic)
instance Default TextLayout where
    def = TextLayout
        { field_textAlign     = TextAlign_Left
        , field_size          = def
        , field_minLineHeight = 0
        , field_content       = []
        }

--------------------------------------------------------------------------------

{-
newtype SizeInPixels = SPix Int

partsPerPixel :: Int
partsPerPixel = 64

--}
