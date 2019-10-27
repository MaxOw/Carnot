-- {-# Options_GHC -fno-warn-unused-imports #-}
{-# Options_GHC -fno-warn-orphans #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Engine.Layout.Alt
    ( Layout
    , fillColor, fillColorA, composition, textline
    , space, container
    , makeRenderLayout, drawLayout

    ,  rel,  seprel,  sep,  cat
    , hrel, hseprel, hsep, hcat
    , vrel, vseprel, vsep, vcat

    , flex, uniflex, uniflexJ
    , text, textJ, textStyled
    , border, borderU, border1

    , (@@), fill, px
    , size, align, padding
    , top, left, right, bottom
    , width, height
    , horizontal, vertical
    , color, alpha
    , absolute

    , makeFontStyle

    , Default (..)
    , Sizing
    , BoxAlign (..), VerticalAlign(..), HorizontalAlign(..)
    , module Engine.HasPattern
    ) where

import Relude
import Data.Default
import qualified Data.List as List
import Linear
import Control.Lens
-- import qualified Data.Sequence as Seq

import Data.Hashable (hash)
import Engine.FontsManager.Types (FontStyle, makeFontStyle)
import Engine.Context (getFramebufferSize, getTime)
import Engine.Types (Engine, graphics)
-- import Engine.FontsManager.Types
import Engine.Graphics.Types
import Engine.Graphics
import Engine.Common.Types hiding (Sizing(..), fill, px)

import qualified Diagrams.TwoD.Transform as T
import Diagrams.Angle ((@@))
import Engine.Lens.Utils
import Engine.Layout.Alt.Lens
import Engine.HasPattern
import Engine.HasField
    ( HasSize(..), HasWidth, HasHeight, width, height
    , boxAlign
    , shapeType, color
    , bottom, top, left, right
    , justify, content
    , offset
    , padding, minSpaceAdvance
    )

import qualified Engine.Graphics.DrawBatchCache as DrawBatchCache

-- TODO:
-- 1. add spaned text formatting.
-- 2. add newlines text splitting.
-- 3. add spaned text utility function.

import qualified Data.Colour as Color
-- import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

instance Default Bool where def = False

--------------------------------------------------------------------------------

newtype Pixels = Pixels { unPixels :: Float }
    deriving (Generic, Eq, Ord, Num, Fractional, Default, Hashable)
data Sizing
   = Sizing_Fill   Float
   | Sizing_Pixels Pixels
   deriving (Generic)
instance Hashable Sizing

fill :: Prism' Sizing Float
fill = cc#_Sizing_Fill

px :: Prism' Sizing Pixels
px = cc#_Sizing_Pixels

data BorderDesc = BorderDesc
   { field_width :: Sizing
   , field_color :: Color
   , field_alpha :: Float
   } deriving (Generic)
instance HasWidth BorderDesc Sizing
instance Default BorderDesc where
    def = BorderDesc
        { field_width = 0 @@ px
        , field_color = Color.black
        , field_alpha = 1
        }

data FlexOpts = FlexOpts
   { field_spaceWidth      :: Pixels
   , field_lineSpaceHeight :: Pixels
   , field_maxLineHeight   :: Pixels
   , field_justify         :: Bool
   } deriving (Generic)
instance Hashable FlexOpts
instance Default FlexOpts

data WordRequest = WordRequest
   { field_fontStyle :: FontStyle
   , field_content   :: Text
   } deriving (Generic)
instance Hashable WordRequest

data TextRequest = TextRequest
   { field_justify :: Bool
   , field_content :: [WordRequest]
   } deriving (Generic)
instance Default TextRequest
instance Hashable TextRequest

data TextResult = TextResult
   { field_size            :: Size Pixels
   , field_minSpaceAdvance :: Pixels
   , field_action          :: RenderAction
   } deriving (Generic)
instance HasSize TextResult (Size Pixels)

data PrimitiveT a
   = Primitive_FillColor AlphaColor
   | Primitive_Composition [LayoutT a]
   | Primitive_Lineup LineupDirection [(Sizing, LayoutT a)]
   | Primitive_Flex FlexOpts [(Pixels, LayoutT a)]
   | Primitive_Absolute (V2 Sizing) (LayoutT a)
   | Primitive_Text a
   deriving (Generic)
instance Hashable a => Hashable (PrimitiveT a)
-- type Primitive = PrimitiveT TextRequest

data PrimitiveOut
   = PrimitiveOut_FillColor AlphaColor
   | PrimitiveOut_Composition [LayoutOut]
   | PrimitiveOut_RenderAction RenderAction

data LayoutT a = Layout
   { field_size      :: Size Sizing
   , field_align     :: BoxAlign
   , field_padding   :: BoxEdges Sizing
   , field_primitive :: PrimitiveT a
   } deriving (Generic)
instance HasSize   (LayoutT a) (Size Sizing)
instance HasWidth  (LayoutT a) Sizing where width  = size.width
instance HasHeight (LayoutT a) Sizing where height = size.height
instance Hashable a => Hashable (LayoutT a)
type Layout       = LayoutT TextRequest
type LayoutResult = LayoutT TextResult

instance Semigroup Layout where sconcat = composition . toList
instance Monoid    Layout where mempty  = def

instance Default (LayoutT a) where
    def = Layout
        { field_size      = pure (1 @@ fill)
        , field_align     = Center
        , field_padding   = pure (0 @@ px)
        , field_primitive = Primitive_Composition []
        }

data LayoutOut = LayoutOut
   { field_rect      :: Rect Pixels
   , field_primitive :: PrimitiveOut
   } deriving (Generic)

instance Default LayoutOut where
    def = LayoutOut
        { field_rect      = Rect 0 0
        , field_primitive = PrimitiveOut_Composition []
        }

simplePrimitive :: PrimitiveT a -> LayoutT a
simplePrimitive p = def & primitive .~ p

fillColor :: Color -> Layout
fillColor = simplePrimitive . Primitive_FillColor . Color.opaque

fillColorA :: AlphaColor -> Layout
fillColorA = simplePrimitive . Primitive_FillColor

composition :: [Layout] -> Layout
composition ls = simplePrimitive $ Primitive_Composition ls

textline :: FontStyle -> Text -> Layout
textline fs t = simplePrimitive $ Primitive_Text $ def
    & ff#content .~ [WordRequest fs t]

container :: Layout -> Layout
container = composition . (:[])

space :: Sizing -> Layout
space r = def & set size (pure r)

distributePx :: Pixels -> [Sizing] -> [Pixels]
distributePx full ls
    | full <= 0     = []
    | full <= allPx = map scalePx ls
    | otherwise     = map toPx ls
    where
    allPx = sumOf (traverse.cc#_Sizing_Pixels) ls
    allFi = sumOf (traverse.cc#_Sizing_Fill)   ls
    leftover = max 0 $ full - allPx
    -- s = allPx / full
    scalePx = \case
        Sizing_Pixels p -> p -- /s
        Sizing_Fill   _ -> 0
    toPx = \case
        Sizing_Pixels p -> p
        Sizing_Fill   f -> leftover * (Pixels $ max 0 $ min 1 $ f/allFi)

resolveLayout :: Layout -> Engine us LayoutResult
resolveLayout = go
    where
    renderTextRequest r = do
        (desc, rend) <- makeRenderTextLine MiddleLeft
            (r^.ff#fontStyle) (r^.ff#content)
        return $ TextResult
            (fmap Pixels $ desc^.size) (Pixels $ desc^.minSpaceAdvance) rend

    renderTextPrimitive x ts = renderTextWords x (ts^.justify) (ts^.content)

    renderTextWords _ _ [ ] = return def
    renderTextWords x _ [a] = reset x . Primitive_Text =<< renderTextRequest a
    renderTextWords x j tts = do
        rts <- mapM renderTextRequest tts
        let spaceSize     = fromMaybe 0 $ maximumOf (traverse.minSpaceAdvance) rts
        let maxLineHeight = fromMaybe 0 $ maximumOf (traverse.size.height) rts
        let opts = def
                 & ff#spaceWidth      .~ spaceSize
                 & ff#lineSpaceHeight .~ 0
                 & ff#maxLineHeight   .~ maxLineHeight
                 & ff#justify         .~ j

        let rs = map (\r -> (r^.size.width, def & primitive .~ Primitive_Text r)) rts
        let p = flex opts rs
        reset x (p^.primitive)

    go x = case x^.primitive of
        Primitive_FillColor   cl -> reset x $ Primitive_FillColor cl
        Primitive_Composition ls -> reset x . Primitive_Composition =<< mapM go  ls
        Primitive_Lineup  dir ls -> reset x . Primitive_Lineup dir  =<< mapM gos ls
        Primitive_Flex   opts ls -> reset x . Primitive_Flex  opts  =<< mapM gos ls
        Primitive_Absolute   p l -> reset x . Primitive_Absolute p =<< go l
        Primitive_Text        ts -> renderTextPrimitive x ts
        where
        gos (a, b) = (a,) <$> go b

    reset x p = return (x :: Layout) { field_primitive = p }

computeLayout :: Size Pixels -> LayoutResult -> LayoutOut
computeLayout windowSize = go (Rect 0 windowSize)
    where
    go parentRect x = case x^.primitive of
        Primitive_FillColor   cl -> computeFillColor   layoutRect cl
        Primitive_Composition ls -> computeComposition layoutRect ls
        Primitive_Lineup  dir ls -> computeLineup  dir layoutRect ls (x^.align)
        Primitive_Flex   opts ls -> computeFlex   opts layoutRect ls (x^.align)
        Primitive_Absolute   p l -> computeAbsolute  p x l
        Primitive_Text         t -> computeText       (textRect t) t
        -- Primitive_Text         t -> computeText        layoutRect t
        where
        layoutRect = computeLayoutRect parentRect x
        textRect t = computeLayoutRect parentRect (x & size .~ ((@@ px) <$> t^.size))
        -- textRect t = computeLayoutRect parentRect x

    computeFillColor outRect col
        = LayoutOut outRect
        $ PrimitiveOut_FillColor col

    computeComposition outRect ls
        = LayoutOut outRect
        $ PrimitiveOut_Composition $ map (go outRect) ls

    distr :: Pixels -> [(Sizing, a)] -> [(Pixels, a)]
    distr full = uncurry zip . over _1 (distributePx full) . unzip

    computeLineup dir outRect ls alg
        = LayoutOut outRect
        $ PrimitiveOut_Composition
        $ map (uncurry go) $ foldLineup fs alg dir outRect dd
        where
        fs = sumOf (traverse._1) dd
        dd = case dir of
            LineupDirection_Horizontal -> distr (outRect^.size.width)  ls
            LineupDirection_Vertical   -> distr (outRect^.size.height) ls

    foldLineup fs alg dir fullRect = f 0
        where
        f _       []  = []
        f !acc ((r,x):xs)
            | checkOverflow (acc+r) = [] -- [(compoff acc r, x)]
            | otherwise = (compoff acc r, x) : f (acc+r) xs

        checkOverflow x =
            floor @_ @Int (unPixels x) > ceiling (unPixels fullExtent)

        compoff acc r = case dir of
            LineupDirection_Horizontal -> fullRect
                & offset._x  %~ offh acc r
                & size.width .~ r
            LineupDirection_Vertical   -> fullRect
                & offset._y  %~ offv acc r
                & size.height .~ r

        fullExtent = case dir of
            LineupDirection_Horizontal -> fullRect^.size.width
            LineupDirection_Vertical   -> fullRect^.size.height

        offh acc r x
            | fs >= fullExtent = x + acc - fullExtent/2 + r/2
            | otherwise = case alg^.horizontal of
                Align_Left   -> x + acc - fullExtent/2 + r/2
                Align_Center -> x + acc - fs/2 + r/2
                Align_Right  -> x - acc + fullExtent/2 - r/2

        offv acc r x
            | fs >= fullExtent = x - acc + fullExtent/2 - r/2
            | otherwise = case alg^.vertical of
                Align_Top    -> x - acc + fullExtent/2 - r/2
                Align_Middle -> x - acc + fs/2 - r/2
                Align_Bottom -> x + acc - fullExtent/2 + r/2

    computeFlex opts outRect ls alg
        | maxLineHeight <= 0 = def
        | otherwise
            = LayoutOut outRect
            $ PrimitiveOut_Composition
            $ map (uncurry go) $ concatMap placeLine $ zip @Int [0..] dls
        where
        spaceWidth      = opts^.ff#spaceWidth
        lineSpaceHeight = opts^.ff#lineSpaceHeight
        maxLineHeight   = opts^.ff#maxLineHeight
        maxWidth        = outRect^.size.width

        fullLineHeight = maxLineHeight + lineSpaceHeight
        fullHeight = genericLength dls * fullLineHeight - lineSpaceHeight

        calcLineWidth = sumOf traverse . intersperse spaceWidth . map (view _1)

        fullWidth = case dls of
            [ ] -> 0
            [_] -> calcLineWidth ls
            _   -> maxWidth

        alignOffsetX = case alg^.horizontal of
            Align_Left   -> maxWidth/2
            Align_Center -> fullWidth/2
            Align_Right  -> -maxWidth/2 + fullWidth

        alignOffsetY = case alg^.vertical of
                Align_Top    -> outRect^.size.height/2
                Align_Middle -> fullHeight/2
                Align_Bottom -> -outRect^.size.height/2 + fullHeight

        dls = distribLines maxWidth spaceWidth ls

        placeLine (i, ll) = foldFlex juo hal ((fromIntegral i)*fullLineHeight) 0 ll
            where
            ofs = maxWidth - calcLineWidth ll
            juo = if i < length dls - 1 then ofs / (genericLength ll - 1) else 0
            hal = if opts^.ff#justify then 0 else case alg^.horizontal of
                Align_Left   -> 0
                Align_Center -> ofs/2
                Align_Right  -> ofs

        foldFlex _   _   _  _    [] = []
        foldFlex juo hal lh !acc ((w, l):xs)
            = (calcFlexRect hal lh acc w, l) : foldFlex juo hal lh newAcc xs
            where
            newAcc = acc + w + opts^.ff#spaceWidth
                   + if opts^.ff#justify then juo else 0

        calcFlexRect hal lh acc w = outRect
            & offset._x +~ acc + w/2 - alignOffsetX + hal
            & offset._y -~ lh  + h/2 - alignOffsetY
            & size      .~ Size w h
            where
            h = maxLineHeight

    computeAbsolute p x l
        = LayoutOut outRect
        $ PrimitiveOut_Composition [go outRect l]
        where
        -- outRect = calcAbsoluteRect p (x^.size) windowSize (x^.align)
        outRect = computeLayoutRect (Rect 0 windowSize) x
            & offset +~ calcAbsolutePos p windowSize
            -- & size .~ x^.size

    computeText outRect t
        = LayoutOut outRect -- rr
        $ PrimitiveOut_RenderAction $ T.translateX xf (t^.ff#action)
        where
        xf = negate $ unPixels $ t^.size.width/2
        -- xf = negate $ unPixels $ t^.size.width/2
        -- rr = outRect & size .~ t^.size

calcAbsolutePos :: V2 Sizing -> Size Pixels -> V2 Pixels
calcAbsolutePos pos windowSize = outPos
    where
    Size w h = windowSize
    outPos  = V2 (toPx w (pos^._x)) (toPx h (pos^._y)) -- + off
    -- outSize = max 0 <$> Size (toPx w (siz^.width)) (toPx h (siz^.height))
    toPx r  = \case
        Sizing_Pixels x -> x
        Sizing_Fill   f -> r * Pixels f

distribLines :: Pixels -> Pixels -> [(Pixels, a)] -> [[(Pixels, a)]]
distribLines _        _          [] = []
distribLines maxWidth spaceWidth ls = ww : distribLines maxWidth spaceWidth oo
    where
    ww = map snd win
    oo = map snd wout
    (win, wout) = List.span ((< maxWidth) . fst) $ zip offs ls
    offs   = zipWith (+) widths spaces
    widths = drop 1 $ scanl (+) 0 $ map fst ls
    spaces = scanl (+) 0 $ repeat spaceWidth

computeLayoutRect :: Rect Pixels -> LayoutT a -> Rect Pixels
computeLayoutRect parentRect l = Rect outOffset outSize
    where
    outSize = baseOutSize - Size hsum vsum
    baseOutSize = toPx <$> parentRect^.size <*> l^.size
    toPx r = \case
        Sizing_Pixels p -> max 0 $ min r p
        Sizing_Fill   f -> r * (Pixels $ max 0 $ min 1 f)
    outOffset = parentRect^.offset + alignVector + (maro/2)
    maro = V2 (pxmr^.left - pxmr^.right) (pxmr^.bottom - pxmr^.top)
    pxmr = onlyPx <$> l^.padding
    vsum = sumOf traverseHorizontal pxmr
    hsum = sumOf traverseVertical   pxmr
    onlyPx = \case
        Sizing_Pixels p -> p
        Sizing_Fill   _ -> 0

    alignVector = (V2 hh vv * lo)
    MkSize lo = parentRect^.size - baseOutSize
    hh = case l^.align.horizontal of
        Align_Left   -> -0.5
        Align_Center -> 0
        Align_Right  -> 0.5

    vv = case l^.align.vertical of
        Align_Top    -> 0.5
        Align_Middle -> 0
        Align_Bottom -> -0.5

makeRenderLayout :: Layout -> Engine us RenderAction
makeRenderLayout layoutIn = do
    canvasSize <- getCanvasSize
    let MkSize v = fmap unPixels canvasSize
    layoutResult <- resolveLayout layoutIn
    let layoutOut = computeLayout canvasSize layoutResult
    makeRenderLayoutOut layoutOut
        <&> T.translate (v/2)

drawLayout :: Layout -> Engine us ()
drawLayout lay = do
    projM <- orthoProjection $ def & boxAlign .~ BottomLeft
    dbc <- use $ graphics.drawBatchCache
    batchAndPrep <- getBatchAndPrepIO
    canvasSize <- getCanvasSize
    let lid = hash (canvasSize, lay)
    let toBatch = liftIO . batchAndPrep <=< makeRenderLayout
    drawBatch projM =<< DrawBatchCache.retriveOrAdd dbc lid lay toBatch

makeRenderLayoutOut :: LayoutOut -> Engine us RenderAction
makeRenderLayoutOut layoutOut = go (layoutOut^.rect) (layoutOut^.primitive)
    where
    go containerRect = \case
        PrimitiveOut_FillColor   cl -> renderFillColor containerRect cl
        PrimitiveOut_Composition ls -> do
            rs <- mapM makeRenderLayoutOut ls
            return $ renderComposition rs
        PrimitiveOut_RenderAction ra -> return $ ra
            & T.translate (unPixels <$> containerRect^.offset)

    renderFillColor containerRect c = return $
        if r^.size.width > 0 && r^.size.height > 0
        then renderColorShape
        else mempty
        where
        r = fmap unPixels containerRect
        renderColorShape = renderShape $ def
            & shapeType .~ SimpleSquare
            & color     .~ c
            & T.scaleX  (r^.size.width)
            & T.scaleY  (r^.size.height)
            & T.translate (r^.offset)

getCanvasSize :: Engine us (Size Pixels)
getCanvasSize = do
    canvasSize <- getFramebufferSize =<< use (graphics.context)
    return . uncurry Size $ over each (Pixels . fromIntegral) canvasSize

--------------------------------------------------------------------------------

rel :: LineupDirection -> [(Sizing, Layout)] -> Layout
rel dir ls = simplePrimitive $ Primitive_Lineup dir ls

sep :: LineupDirection -> Sizing -> [Layout] -> Layout
sep dir s = rel dir . intersperse (s, def) . map (1 @@ fill,)

seprel :: LineupDirection -> Sizing -> [(Sizing, Layout)] -> Layout
seprel dir s = rel dir . intersperse (s, def)

cat :: LineupDirection -> [Layout] -> Layout
cat dir = rel dir . map (1 @@ fill,)

--------------------------------------------------------------------------------

hrel :: [(Sizing, Layout)] -> Layout
hrel = rel Horizontal

hseprel :: Sizing -> [(Sizing, Layout)] -> Layout
hseprel = seprel Horizontal

hcat :: [Layout] -> Layout
hcat = cat Horizontal

hsep :: Sizing -> [Layout] -> Layout
hsep = sep Horizontal

--------------------------------------------------------------------------------

vrel :: [(Sizing, Layout)] -> Layout
vrel = rel Vertical

vseprel :: Sizing -> [(Sizing, Layout)] -> Layout
vseprel s = seprel Vertical s

vcat :: [Layout] -> Layout
vcat = cat Vertical

vsep :: Sizing -> [Layout] -> Layout
vsep s = sep Vertical s

--------------------------------------------------------------------------------

flex :: FlexOpts -> [(Pixels, LayoutT a)] -> LayoutT a
flex opts ls = simplePrimitive $ Primitive_Flex opts ls

uniflex :: Pixels -> Pixels -> [(Pixels, LayoutT a)] -> LayoutT a
uniflex spaceSize maxLineHeight = flex opts
    where
    opts = def
        & ff#spaceWidth      .~ spaceSize
        & ff#lineSpaceHeight .~ spaceSize
        & ff#maxLineHeight   .~ maxLineHeight

uniflexJ :: Pixels -> Pixels -> [(Pixels, LayoutT a)] -> LayoutT a
uniflexJ spaceSize maxLineHeight = flex opts
    where
    opts = def
        & ff#spaceWidth      .~ spaceSize
        & ff#lineSpaceHeight .~ spaceSize
        & ff#maxLineHeight   .~ maxLineHeight
        & ff#justify         .~ True

--------------------------------------------------------------------------------

textStyled :: [(FontStyle, Text)] -> Layout
textStyled [] = def
textStyled ls = simplePrimitive $ Primitive_Text $ def
    & content .~ concatMap f ls
    where
    f (s,t) = map (WordRequest s) $ words t

text :: FontStyle -> Text -> Layout
text fs t = simplePrimitive $ Primitive_Text $ def
    & content .~ map (WordRequest fs) ws
    where
    ws = words t

textJ :: FontStyle -> Text -> Layout
textJ fs t = simplePrimitive $ Primitive_Text $ def
    & content .~ map (WordRequest fs) ws
    & justify .~ True
    where
    ws = words t

--------------------------------------------------------------------------------

border :: BoxEdges BorderDesc -> Layout -> Layout
border desc ins = vrel
    [ topb
    , (1 @@ fill, hrel [ leftb, center, rightb])
    , bottomb ]
    where
    mkb s   = (s^.width, fillColorA $ Color.withOpacity (s^.color) (s^.alpha))
    topb    = mkb $ desc^.top
    bottomb = mkb $ desc^.bottom
    leftb   = mkb $ desc^.left
    rightb  = mkb $ desc^.right
    center  = (1 @@ fill, ins)

borderU :: Sizing -> Color -> Layout -> Layout
borderU s c = border $ def & each.width .~ s & each.color .~ c

border1 :: Color -> Layout -> Layout
border1 = borderU (1 @@ px)

--------------------------------------------------------------------------------

absolute :: V2 Sizing -> Layout -> Layout
absolute p = simplePrimitive . Primitive_Absolute p

