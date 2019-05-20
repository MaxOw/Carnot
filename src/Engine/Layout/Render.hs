{-# Language TypeFamilies #-}
module Engine.Layout.Render where

import Delude

import Diagrams.Core (InSpace, Transformable)
import qualified Diagrams.TwoD.Transform as T

import Engine.Layout.Types
import Engine.Graphics
import Engine (Engine, graphics, getFramebufferSize)

import Engine.Layout.Alt ()

--------------------------------------------------------------------------------

getCanvasSize :: Engine us (Size AbsoluteSize)
getCanvasSize = do
    canvasSize <- getFramebufferSize =<< use (graphics.context)
    return . uncurry Size $ over each fromIntegral canvasSize

makeRenderLayout :: Layout -> Engine us RenderAction
makeRenderLayout layout = do
    canvasSize <- getCanvasSize
    go canvasSize canvasSize layout
    where
    boxModel = True
    go canvasSize containerSize = \case
        Layout_Box desc contents -> do
            let opts = toSimpleBoxOpts canvasSize containerSize desc
            let contentSize = toContentSize boxModel desc opts
            let t = boxAlignTransform containerSize (opts^.size) (desc^.boxAlign)
            let r = renderComposition . (renderSimpleBox opts :)
            let p = T.translate $ paddingOffset $ desc^.padding
            t . r . p <$> mapM (go canvasSize contentSize) contents
        Layout_Lineup desc items -> do
            let (off, bs) = calcLineupBBoxes canvasSize containerSize desc items
            let rend = renderLineup desc containerSize off
            let itemsFixed = map (fixLineupBoxSize $ desc^.direction) items
            rend <$> zipWithM (renderLineupItem desc canvasSize) bs itemsFixed
        Layout_Text desc richText -> do
            let textLayout = def
                    & textAlign     .~ (desc^.textAlign)
                    & minLineHeight .~ (desc^.minLineHeight)
                    & size          .~ containerSize
                    & content       .~ richText
            renderTextLayout <- makeRenderTextLayout textLayout
            let outputSize = renderTextLayout^.size
            let tv = V2 (-outputSize^.width/2) (outputSize^.height/2)
            let t = boxAlignTransform containerSize outputSize (desc^.boxAlign)
            t . T.translate tv <$> return (view renderAction renderTextLayout)
        Layout_Empty -> return mempty

    paddingOffset p = V2 (p^.left - p^.right) (p^.bottom - p^.top)

    fixLineupBoxSize dir = \case
        Layout_Box desc cs -> case dir of
            LineupDirection_Horizontal ->
                Layout_Box (desc & size.width  .~ 1 @@ fill) cs
            LineupDirection_Vertical   ->
                Layout_Box (desc & size.height .~ 1 @@ fill) cs
        other              -> other

    renderLineup desc containerSize off = trans . renderComposition
        where
        trans = case desc^.direction of
            LineupDirection_Horizontal ->
                T.translateX (-containerSize^.width /2 + justifyOffset)
            LineupDirection_Vertical   ->
                T.translateY ( containerSize^.height/2 - justifyOffset)

        justifyOffset = case desc^.justify of
            LineupJustify_Start        -> 0
            LineupJustify_End          -> off
            LineupJustify_Center       -> off/2
            LineupJustify_SpaceBetween -> 0

    renderLineupItem desc canvasSize bbox item
        = trans <$> go canvasSize (bbox^.size) item
        where
        trans = T.translate (bbox^.offset) . case desc^.direction of
          LineupDirection_Horizontal -> T.translateX ( bbox^.size.width /2)
          LineupDirection_Vertical   -> T.translateY (-bbox^.size.height/2)

calcLineupBBoxes
    :: Size AbsoluteSize -> Size AbsoluteSize
    -> LineupDesc -> [Layout] -> (AbsoluteSize, [Rect AbsoluteSize])
calcLineupBBoxes canvasSize containerSize desc ls
    = (sizeDir leftoverSize, foldOff 0 finalSizes)
    where
    calcSizes = map (calcBoxSize canvasSize containerSize fillSize)
    sizes = calcSizes baseSizes

    baseSizes = map getLayoutSize ls
    fillSize = makeSizeDir 0 dir $ sizeDir $ Size
        (sumOf (traverse.width .fill) baseSizes)
        (sumOf (traverse.height.fill) baseSizes)

    propSizes = calcSizes $ set (traverse.traverse.fill) 0 baseSizes
    fullSize = Size
        (sumOf (traverse.width)  propSizes)
        (sumOf (traverse.height) propSizes)

    leftoverSize = containerSize - fullSize

    dir = desc^.direction
    sizeDir x = case dir of
        LineupDirection_Horizontal -> x^.width
        LineupDirection_Vertical   -> x^.height

    finalSizes = map contSize $ if sizeDir leftoverSize < 0
        then zipWith fixSize propSizes sizes
        else sizes

    contSize x = case dir of
        LineupDirection_Horizontal -> Size (x^.width) (containerSize^.height)
        LineupDirection_Vertical   -> Size (containerSize^.width) (x^.height)

    fixSize p s = case dir of
        LineupDirection_Horizontal -> Size fixed (s^.height)
            where fixed = containerSize^.width * (p^.width / fullSize^.width)
        LineupDirection_Vertical   -> Size (s^.width) fixed
            where fixed = containerSize^.height * (p^.height / fullSize^.height)

    spaceAdv = case desc^.justify of
        LineupJustify_SpaceBetween | sizeDir fillSize == 0
          -> max 0 (sizeDir leftoverSize / (genericLength ls - 1))
        _ -> 0

    foldOff _    []     = []
    foldOff !acc (s:sz) = case desc^.direction of
        LineupDirection_Horizontal -> Rect (V2 acc 0) s
            : foldOff (acc + s^.width + spaceAdv) sz
        LineupDirection_Vertical   -> Rect (V2 0 (-acc)) s
            : foldOff (acc + s^.height + spaceAdv) sz

makeSizeDir :: a -> LineupDirection -> a -> Size a
makeSizeDir d dir v = case dir of
    LineupDirection_Horizontal -> Size v d
    LineupDirection_Vertical   -> Size d v

getLayoutSize :: Layout -> Size Sizing
getLayoutSize = \case
    Layout_Box    d _ -> d^.size
    _                 -> pure (1 @@ fill)

toSimpleBoxOpts
    :: Size AbsoluteSize -> Size AbsoluteSize -> BoxDesc -> SimpleBoxOpts
toSimpleBoxOpts canvasSize containerSize desc = SimpleBoxOpts
   { field_size   = absSize
   , field_color  = desc^.color
   , field_border = desc^.border
   }
   where
   absSize = calcBoxSize canvasSize containerSize 1 $ desc^.size

toContentSize :: Bool -> BoxDesc -> SimpleBoxOpts -> Size AbsoluteSize
toContentSize boxModel desc opts = if boxModel
    then opts^.size - paddingSize - borderSize
    else opts^.size - paddingSize
    where
    paddingSize = Size
        (sumOf (padding.traverseHorizontal) desc)
        (sumOf (padding.traverseVertical  ) desc)
    borderSize = Size
        (sumOf (border.traverseHorizontal.width) desc)
        (sumOf (border.traverseVertical  .width) desc)

boxAlignTransform
    :: (InSpace V2 AbsoluteSize t, Transformable t)
    => Size AbsoluteSize -> Size AbsoluteSize -> BoxAlign -> t -> t
boxAlignTransform containerSize contentSize align
    = T.translate (       containerSize^._Wrapped * boxAlignVector align)
    . T.translate (negate $ contentSize^._Wrapped * boxAlignVector align)

calcBoxSize
    :: Size AbsoluteSize
    -> Size AbsoluteSize
    -> Size FillPart
    -> Size Sizing
    -> Size AbsoluteSize
calcBoxSize canvasSize containerSize fillSize s = def
   & width  .~ calc width  (s^.width)  (fillSize^.width)
   & height .~ calc height (s^.height) (fillSize^.height)
   where
   clamp a b = max a . min b
   calc dir v f = clamp 0 x $ case v of
        Sizing_ContainerPct pct -> x * pct
        Sizing_WindowPct    pct -> c * pct
        Sizing_Absolute     apx -> apx
        Sizing_Fill         fil -> x * (fil/f)
        where
        c = canvasSize^.dir
        x = containerSize^.dir

renderSimpleBox :: SimpleBoxOpts -> RenderAction
renderSimpleBox box = renderComposition [ mempty
    , renderBackground
    , renderBorderTop
    , renderBorderBottom
    , renderBorderLeft
    , renderBorderRight
    ]
    where
    b = box^.border
    background = Size
        (box^.size.width  - sumOf (traverseHorizontal.width) b)
        (box^.size.height - sumOf (traverseVertical  .width) b)
    halfP =          (*0.5)
    halfN = negate . (*0.5)

    renderBackground
        = T.scaleX     (background^.width)
        $ T.scaleY     (background^.height)
        $ renderSquare (box^.color)

    renderBorderTop
        = renderIf (b^.top.width > 0)
        $ T.translateY (halfP $ background^.height + b^.top.width)
        $ T.scaleX     (box^.size.width)
        $ T.scaleY     (b^.top.width)
        $ renderSquare (b^.top.color)

    renderBorderBottom
        = renderIf (b^.bottom.width > 0)
        $ T.translateY (halfN $ background^.height + b^.bottom.width)
        $ T.scaleX     (box^.size.width)
        $ T.scaleY     (b^.bottom.width)
        $ renderSquare (b^.bottom.color)

    renderBorderLeft
        = renderIf (b^.left.width > 0)
        $ T.translateX (halfN $ background^.width + b^.left.width)
        $ T.scaleX     (b^.left.width)
        $ T.scaleY     (background^.height)
        $ renderSquare (b^.left.color)

    renderBorderRight
        = renderIf (b^.right.width > 0)
        $ T.translateX (halfP $ background^.width + b^.right.width)
        $ T.scaleX     (b^.right.width)
        $ T.scaleY     (background^.height)
        $ renderSquare (b^.right.color)

    renderSquare c
        = renderShape $ def
            & shapeType .~ SimpleSquare
            & color     .~ c

    renderIf False _ = mempty
    renderIf True  r = r

