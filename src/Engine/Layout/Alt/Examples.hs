module Engine.Layout.Alt.Examples where

import Relude
import Control.Lens hiding (para)
import Engine.Layout.Alt
import Linear (V2(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.Colour as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

padding1 :: Layout
padding1 = fillColor Color.red
    & padding.bottom .~ 20 @@ px
    & padding.top .~ 90 @@ px
    & padding.left .~ 90 @@ px
    & padding.right .~ 10 @@ px

box1 :: Layout
box1 = fillColor Color.red
    & size.each    .~ 0.5 @@ fill
    & align        .~ BottomRight
    & padding.each .~ 20 @@ px

boxRow1 :: Layout
boxRow1 = hcat rs
    & size  .~ pure (0.8 @@ fill)
    & align .~ MiddleLeft
    where
    rs = map f [ Color.red, Color.green, Color.blue ]
    f = fillColor

boxRowSep1 :: Layout
boxRowSep1 = hsep (20 @@ px) cs
    & padding.each .~ 20 @@ px
    & size        .~ pure (0.5 @@ fill)
    & align       .~ TopLeft
    where
    cs = map f [ Color.red, Color.green, Color.blue ]
    f = fillColor

boxRowRel1 :: Layout
boxRowRel1 = hrel
    [ (2 @@ fill, fillColor Color.red)
    , (1 @@ fill, fillColor Color.green)
    , (20  @@ px, fillColor Color.blue)
    ]
    & padding.each .~ 20 @@ px
    & size        .~ pure (0.5 @@ fill)
    & align       .~ TopCenter

complex1 :: Layout
complex1 = vrel
    [ (40 @@ px , titleBar)
    , (1 @@ fill, body)
    , (10 @@ px , fillColor Color.darkgrey)
    ]
    -- & size        .~ pure (0.9 @@ fill)
    & padding.each .~ 20 @@ px
    where
    titleBar = composition
        [ fillColor Color.grey
        , fillColor Color.darkgrey
            & align       .~ MiddleLeft
            & padding.each .~  8 @@ px
            & size.width .~ 0.6 @@ fill
        , fillColor Color.darkgrey
            & align       .~ MiddleRight
            & padding.each .~  8 @@ px
            & size.width  .~ 40 @@ px
        ]

    body = composition
        [ fillColor Color.lightgray
        , hsep (8 @@ px) [ leftsize , rightside ]
        ]

    leftsize = vseprel (8 @@ px)
        (replicate 10 (26 @@ px, fillColor Color.darkgrey))
        & padding.each .~ 8 @@ px
        & padding.right .~ 0 @@ px
        & align .~ TopLeft

    rightside = composition
        [ fillColor Color.darkgrey
        ]
        & padding.each .~ 8 @@ px
        & padding.left .~ 0 @@ px

uniflex1 :: Layout
uniflex1 = composition
    [ fillColor Color.lightgray
    , vsep (8 @@ px)
        [ para & align .~ TopLeft
        , para
        , paraJ
        , para & align .~ BottomRight
        ]
    ]
    & padding.each .~ 20 @@ px

    where
    para  = uniflex  8 20 wds & padding.each .~ 20 @@ px
    paraJ = uniflexJ 8 20 wds & padding.each .~ 20 @@ px

    wds   = zip sizes $ repeat $ fillColor Color.darkgray
    sizes = take 23 $ cycle [ 70, 20, 50, 80, 30 ]

    -- sizes = take 24 $ cycle [ 140, 30, 80, 300, 180, 20 ]
    -- sizes = take 2 $ cycle [ 80, 90 ]
    -- sizes = take 3 $ cycle [ 80, 90 ]

text1 :: Layout
text1 = composition
    [ fillColor Color.lightgray
    , textline fs "Áest" & align .~ TopLeft
    , textline fs "TesÓ" & align .~ TopRight
    , textline fs "yest" & align .~ BottomLeft
    , textline fs "Tesy" & align .~ BottomRight
    , textline fs "This is test"
    ]
    & padding.each .~ 20 @@ px
    where
    fs = makeFontStyle ["Arial"] 12
       -- & color .~ Color.opaque Color.gray
       & color .~ Color.opaque Color.white

textPadding1 :: Layout
textPadding1 = textline fs "This is test"
    & align .~ BottomRight -- Center
    -- & align .~ TopLeft
    -- & padding.each .~ 80 @@ px
    -- & size .~ pure (0.5 @@ fill)
    -- & align .~ BottomRight
    -- & align .~ TopLeft
    where
    fs = makeFontStyle ["Arial"] 20
       & color .~ Color.opaque Color.gray

bigtext1 :: Layout
bigtext1 = composition
    [ fillColor Color.lightgray
    , vsep (1 @@ fill)
        [ text  fs loremipsum & align .~ TopLeft
        , text  fs loremipsum
        , textJ fs loremipsum
        , text  fs loremipsum & align .~ BottomRight
        ]
        & padding.each .~ 20 @@ px
    ]
    -- & size .~ pure (0.5 @@ fill)
    & padding.each .~ 20 @@ px
    where
    fs = makeFontStyle ["Arial"] 12
       & color .~ Color.opaque Color.gray

basePadding :: Sizing
basePadding = 20 @@ px

offi :: Layout
offi = vseprel (8 @@ px)
    -- [ (1 @@ fill, border1 Color.gray ins)
    [ (1 @@ fill, question)
    , (30 @@ px, answer)
    , (30 @@ px, runeload)
    ]
    & padding.each .~ basePadding
    & size.width   .~ (0.5 @@ fill)
    & size.height  .~ (0.45 @@ fill)
    & align        .~ BottomLeft
    where
    answer = border1 Color.gray $ composition
        [ fillColor Color.lightgray
        , text fs "Blue▏" -- ▎
            & align .~ MiddleLeft
            & padding.each .~ 8 @@ px
            & padding.left .~ 20 @@ px
        ]

    question = composition
        [ fillColor Color.lightgray
        , text fs "What is your favorite color?"
            & align .~ TopLeft
            & padding.each .~ 4 @@ px
        ]

    runeload = hseprel (8 @@ px) runes
        & align .~ TopLeft

    runes = replicate 4 (30 @@ px, fillColor Color.red)

    fs = makeFontStyle ["Arial", "SourceHanSerif"] 10
       & color .~ Color.opaque Color.gray

absolute1 :: Layout
absolute1 = absolute pos (boxRowRel1 & size.each .~ 1 @@ fill) -- fillColor Color.red)
    & size.each .~ (200 @@ px)
    -- & align .~ TopLeft
    -- & align .~ BottomRight
    where
    pos = V2 (-50 @@ px) (-50 @@ px)
    -- pos = pure (0 @@ px) -- V2 (20 @@ px) (50 @@ px)
    -- pos = pure (-50 @@ px) -- V2 (20 @@ px) (50 @@ px)
    -- pos = V2 (-20 @@ px) (0 @@ px)

textStyled1 :: Layout
textStyled1 = textStyled
    [ (makeFs Color.red, "Test ") ]
    -- [ (makeFs Color.black, "This is some")
    -- , (makeFs Color.red  , "random text")
    -- , (makeFs Color.black, "like this.")
    -- ]

-- makeFs :: Color -> FontStyle
makeFs c = makeFontStyle ["Arial", "SourceHanSerif"] 10
       & color .~ Color.opaque c

--------------------------------------------------------------------------------

examples :: Vector Layout
examples = Vector.fromList
    [ text1
    , textStyled1
    , absolute1
    , padding1
    , box1
    , boxRow1
    , boxRowSep1
    , boxRowRel1
    , complex1
    , uniflex1
    -- , text1
    , bigtext1
    , textPadding1
    , offi
    ]

selectExample :: Int -> Layout
selectExample = Vector.unsafeIndex examples . limitExampleIx

limitExampleIx :: Int -> Int
limitExampleIx = max 0 . min mm . flip mod mm
    where
    mm = Vector.length examples

--------------------------------------------------------------------------------

loremipsum :: Text
loremipsum = mconcat
    [ "Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut "
    , "odit aut fugit, sed quia consequuntur magni dolores eos qui ratione "
    -- , "voluptatem sequi nesciunt "
    ]

