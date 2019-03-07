{-# Language TypeFamilies #-}
module Engine.Graphics
    ( module Types
    , initGraphics
    , draw
    , drawBatch
    , swapBuffers
    , orthoProjection, orthoProjectionFor
    , positionToViewMatrix
    , fullyUpdateAtlas

    , batchRenderAction

    , pixelsToPoints
    , pointsToPixels
    , transformBoxAlign
    , boxAlignVector

    , loadFont
    , loadFontFamily
    , createFontHierarchy
    -- , makeRenderChar
    -- , makeRenderText
    , makeRenderTextLayout

    , updateZIndex, setZIndex, setZIndexAtLeast, setZIndexAtMost

    , renderShape
    , renderFromAtlas
    , renderComposition
    , renderSimpleText
    , setDefaultFonts

    , renderActionBBox

    , loadTextureToAtlas
    , renderImg
    ) where

import Delude
import Linear
import qualified Data.Sequence as Seq
import qualified Data.Char as Char
import qualified Data.List as List
-- import qualified Data.Vector.Algorithms.Intro as M
-- import qualified Data.Vector.Mutable as M

import Diagrams.Core (InSpace, Transformable)
import qualified Diagrams.Transform as T
import qualified Diagrams.TwoD.Transform as T

import Engine.Graphics.Types as Types
import Engine.Graphics.Draw.Atlas

import Engine.FontsManager
import Engine.Layout.Types

import Engine.Graphics.TextureAtlas (TextureAtlas)
import qualified Engine.Graphics.TextureAtlas as Atlas
import qualified Engine.Context as Context
import Engine.Types
import Engine.Context (Context)
import Engine.Graphics.Utils (loadImageSync, createTextureBufferFrom, mkMatHomo2)
import Codec.Picture (Image(..), convertRGBA8)

--------------------------------------------------------------------------------

initDrawProcedure :: MonadIO m => TextureAtlas -> m DrawProcedure
initDrawProcedure atlas = do
    drawAtlas    <- initDrawAtlas
 -- drawTextures <- initDrawTextures
 -- drawShapes   <- initDrawShapes
 -- drawPolygons <- initDrawPolygons
    return $ \p r -> do
        unlessEmpty (sortZIndex $ r^.requestedAtlas) $ drawAtlas atlas p
     -- unlessEmpty (r^.requestedTextures) $ drawTextures    p
     -- drawShapes      p $ r^.requestedShapes
     -- drawPolygons    p $ r^.requestedPolygons

    where
    unlessEmpty x f
        | isn't _Empty x = f x
        | otherwise      = return ()

    sortZIndex :: AtlasBatch -> AtlasBatch
    sortZIndex = Seq.sortBy $ comparing $ view zindex

batchRenderAction :: RenderAction -> Engine us DrawRequest
batchRenderAction = go def
    where
    go r = \case
        RenderFromAtlas   d    -> return $ over requestedAtlas (|> d) r
        RenderComposition t rs -> foldlM go r (T.transform t rs)
        RenderSimpleText  d tx -> do
            rt <- makeRenderSimpleText d tx
            go r rt
            -- return r

--------------------------------------------------------------------------------

initGraphics :: MonadIO m => Context -> m GraphicsState
initGraphics ctx = do
    atlas <- Atlas.newAtlas
    mDPI <- Context.getPrimaryDPI
    -- logOnce $ "Monitor DPI: " <> show mDPI
    fm <- initFontsManager mDPI
    proc <- initDrawProcedure atlas
    return $ GraphicsState
        { _context          = ctx
        , _drawProcedure    = proc
        , _textureAtlas     = atlas
        , _fontsManager     = fm
        , _defaultFontStyle = Nothing
        }

draw :: Mat4 -> RenderAction -> Engine us ()
draw projMatrix = drawBatch projMatrix <=< batchRenderAction

drawBatch :: Mat4 -> DrawRequest -> Engine us ()
drawBatch projMatrix batch = do
    drawProc <- use $ graphics.drawProcedure
    liftIO $ drawProc projMatrix batch

loadTextureToAtlas :: FilePath -> Engine us (Maybe Img)
loadTextureToAtlas path = do
    atlas <- use $ graphics.textureAtlas
    -- mTex <- loadTexture path
    mImg <- loadImageSync path
    case mImg of
        Nothing  -> return Nothing
        Just img -> do
            buf <- createTextureBufferFrom img
            Atlas.addTexture atlas buf
            let (Image w h _) = convertRGBA8 img
            -- deleteTextureBuffer buf
            let tex = buf^.texture
            return (Just $ mkImg tex (Size w h))

{-
loadTexture :: MonadIO m => FilePath -> m (Maybe Texture)
loadTexture path = do
    mImg <- loadImageSync path
    case mImg of
        Nothing  -> return Nothing
        Just img -> return . Just =<< createTexture2d img
-}

loadFont :: FontName -> FilePath -> Engine us (Maybe Font)
loadFont fname path = do
    fm <- use $ graphics.fontsManager
    managerLoadFont fm fname path

loadFontFamily
    :: FontFamilyName -> FontFamilyDesc -> Engine us (Maybe FontFamily)
loadFontFamily famName desc = do
    fm <- use $ graphics.fontsManager
    managerLoadFontFamily fm famName desc

createFontHierarchy :: [FontFamilyName] -> Engine us FontHierarchy
createFontHierarchy names = do
    fm <- use $ graphics.fontsManager
    managerCreateFontHierarchy fm names

makeDrawCharStep :: FontStyleRes -> Char -> Engine us (Maybe DrawCharStep)
makeDrawCharStep fs char = do
    atlas <- use $ graphics.textureAtlas
    mGlyph <- hierarchy_loadGlyph (fs^.fonts) key
    caseJust mGlyph $ \glyph -> do
        Atlas.addTexture atlas $ glyph^.buffer
        return $ Just $ makeStep glyph

    where
    key = GlyphKey char (fs^.fontSize) (fs^.bold) (fs^.italic)
    makeTrans glyph = mempty
        & T.translate (V2 0.5 (-0.5)) -- center -> top left conrner
        & T.scaleX (pointsToPixels $ glyph^.size._x)
        & T.scaleY (pointsToPixels $ glyph^.size._y)
        & T.translate (over each pointsToPixels $ glyph^.bearing)

    makeStep glyph = DrawCharStep
        { drawCharStep_texture        = glyph^.buffer.texture
        , drawCharStep_color          = fs^.color
        , drawCharStep_modelTransform = makeTrans glyph
        , drawCharStep_advance        = glyph^.advance
        }

-- type Points = Int
-- type Pixels = Float

pointsToPixels :: Int -> Float
pointsToPixels = (/64) . fromIntegral

pixelsToPoints :: Float -> Int
pixelsToPoints = floor . (*64)

makeRenderSimpleText :: SimpleTextDesc -> Text -> Engine us RenderAction
makeRenderSimpleText d text = do
    fs <- uses (graphics.defaultFontStyle) (fromMaybe def)
    let ff = fs
           & over fonts    (maybe id (:)   $ d^.fontName)
           & over fontSize (maybe id const $ d^.fontSize)
           & set  color    (d^.color)
    makeRenderText (d^.boxAlign) ff text

makeRenderText :: BoxAlign -> FontStyle -> Text -> Engine us RenderAction
makeRenderText align fs text = do
    fh  <- retriveHierarchy fs
    case getPrimaryFont (fh^.fonts) (fs^.bold) (fs^.italic) of
        Just f -> do
            m   <- getFontMetrics f (fs^.fontSize)
            dws <- mapM (toDrawCharList fh . toString) $ words text
            let s = fmap pointsToPixels $ Size (calcWidth m dws) (m^.lineHeight)
            return $ transformBoxAlign s align $ vertOff m
                   $ renderComposition $ concat $ go m 0 dws
        Nothing -> return mempty
    where
    vertOff m = T.translateY (pointsToPixels $ negate $ m^.verticalOffset)
    toDrawCharList fh = fmap catMaybes . mapM (makeDrawCharStep fh)

    go _ _       []  = []
    go m !adv (w:ws) = drawWord adv w : go m (adv + off) ws
        where
        off = sumOf (traverse.advance) w + view minSpaceAdvance m

    drawWord _       []  = []
    drawWord !adv (l:ls) = drawChar l : drawWord (adv + off) ls
        where
        drawChar = toRenderChar . over modelTransform trans
        trans    = T.translateX $ pointsToPixels adv
        off      = l^.advance

    calcWidth m ws = (sum $ map (sumOf (traverse.advance)) ws)
                   + (max 0 (length ws-1) * view minSpaceAdvance m)

transformBoxAlign :: (InSpace V2 n t, Fractional n, Transformable t)
    => Size n -> BoxAlign -> t -> t
transformBoxAlign s align = T.translate (s^._Wrapped * vv)
    where
    V2 x y = boxAlignVector align
    vv = V2 (-x-0.5) (0.5-y)

boxAlignVector :: Fractional n => BoxAlign -> V2 n
boxAlignVector align = V2 hori vert
    where
    hori = case align^.horizontal of
        Align_Left   -> -(0.5)
        Align_Center ->   0
        Align_Right  ->   0.5

    vert = case align^.vertical of
        Align_Top    ->   0.5
        Align_Middle ->   0
        Align_Bottom -> -(0.5)

data DrawRichStep
   = DrawRichChar  DrawCharStep
   | DrawRichImg   Texture Advance
   | DrawRichSpace
   | DrawRichInitialSpace
   | DrawRichNewline

makeRenderTextLayout :: TextLayout -> Engine us RenderTextLayout
makeRenderTextLayout t = do
    ms <- mapM (uncurry getFontMetrics) =<< styles
    let mlh        = t^.minLineHeight.to pixelsToPoints
    let maxOf0 l   = fromMaybe 0 . maximumOf (traverse.l)
    let lineheight = max mlh $ maxOf0 lineHeight ms
    let vertoffset = maxOf0 verticalOffset  ms
    let spaceAdv   = maxOf0 minSpaceAdvance ms
    steps <- toRichSteps $ t^.content
    let (outputSize, renderLines) = renderRichSteps
            (t^.textAlign) (t^.size.width) (t^.size.height)
            (pointsToPixels lineheight) spaceAdv steps
    return $ RenderTextLayout
        { renderTextLayout_size         = outputSize
        , renderTextLayout_renderAction = vertOff vertoffset renderLines
        }
    where
    vertOff v = T.translateY (pointsToPixels $ negate v)
    styles = uniq . mapMaybe getStyle $ t^.content
    uniq = fmap simp . mapM retriveHierarchy
    simp = ordNub . mapMaybe (\f -> (,f^.fontSize) <$> getPF f)
    getStyle = \case
        RichText_Span fs _ -> Just fs
        RichText_Image   _ -> Nothing

    getPF f = getPrimaryFont (f^.fonts) (f^.bold) (f^.italic)

    -- toRichSteps :: [RichText] -> [DrawRichStep]
    toRichSteps = fmap concat . mapM toDraw
    -- toRichSteps _ = []

    -- toDraw :: RichText -> [DrawRichStep]
    toDraw :: RichText -> Engine us [DrawRichStep]
    toDraw = \case
        RichText_Span  fs x -> do
            fh <- retriveHierarchy fs
            catMaybes <$> (mapM (toDrawChar fh) $ toString x)
        RichText_Image img  -> return [DrawRichImg (img^.texture) imgAdv]
            where -- TODO: fix this!
            imgAdv = pixelsToPoints $ fromIntegral $ img^.size.width

    -- toDrawChar :: FontStyle -> Char -> DrawRichStep
    toDrawChar fs c
        | c == '\n'        = return $ Just DrawRichNewline
        | cat Char.Space   = return $ Just DrawRichSpace
        | cat Char.Control = return Nothing
        | otherwise        = fmap DrawRichChar <$> makeDrawCharStep fs c
        where
        cat = (Char.generalCategory c ==)

retriveHierarchy :: FontStyle -> Engine us FontStyleRes
retriveHierarchy f = do
    h <- createFontHierarchy (f^.fonts)
    return $ f { fontStyle_fonts = h }

data DrawWordStep
   = DrawWord [DrawRichStep]
   | DrawWordSpace Int

data DrawLineStep
   = DrawLineFull Bool [DrawWordStep]

renderRichSteps
    :: TextAlign
    -> AbsoluteSize
    -> AbsoluteSize
    -> AbsoluteSize
    -> Advance
    -> [DrawRichStep]
    -> (Size AbsoluteSize, RenderAction)
renderRichSteps
    layoutTextAlign
    layoutWidth
    layoutHeight
    layoutLineHeight
    spaceAdv
    steps
    = (outputSize, renderLines properLines)
    where
    plen = length properLines
    outputSize = case properLines of
        []     -> pure 0
        (l:[]) -> Size (calcLineLength l) layoutLineHeight
        -- (l:[]) -> Size layoutWidth layoutLineHeight
        _      -> Size layoutWidth (fromIntegral plen * layoutLineHeight)

    properLines = concatMap splitBaseLine baseLines
    baseLines = splitStepsByNewline steps

    splitStepsByNewline = splitWhen isDrawRichNewline

    splitBaseLine
        = preserveEmptyLines
        . parcelToSize
        . mapMaybe toWordSpace
        . splitWhenKeep isDrawRichSpace
        . markInitialSpaces

    preserveEmptyLines [] = [DrawLineFull False []]
    preserveEmptyLines as = as

    markInitialSpaces ls = inis <> rest
        where
        inis = map (const DrawRichInitialSpace) front
        (front, rest) = List.span isDrawRichSpace ls

    maxAdv = pixelsToPoints layoutWidth

    toWordSpace [] = Nothing
    toWordSpace ls@(DrawRichSpace:_) = Just $ DrawWordSpace (length ls)
    toWordSpace ls = Just $ DrawWord ls

    wordAdvance = \case
        DrawWord      ls -> sum $ map (stepAdvance spaceAdv) ls
        DrawWordSpace ct -> ct * spaceAdv

    isDrawRichNewline DrawRichNewline = True
    isDrawRichNewline _ = False

    isDrawRichSpace DrawRichSpace = True
    isDrawRichSpace _ = False

    isDrawWordSpace (DrawWordSpace {}) = True
    isDrawWordSpace _ = False

    parcelToSize = go 0 []
        where
        go  _ [] [] = []
        go  _ ns [] = DrawLineFull False ns : []
        go !a ns (x:xs)
            | newAdv > maxAdv = DrawLineFull True  ns : xxs
            | otherwise       = go newAdv (ns<>[x]) xs
            where
            newAdv = a + adv
            adv    = wordAdvance x
            xxs    = case x of
                DrawWordSpace _ -> go 0 [] xs
                _ -> if adv > maxAdv
                    then splitWord x (go 0 [] xs)
                    else go 0 [] (x:xs)

    splitWord (DrawWordSpace {}) f = f
    splitWord (DrawWord ls) f = go 0 [] ls
        where
        go  _ []    [] = f
        go  _ cs    [] = DrawLineFull False [DrawWord cs] : f
        go !a cs (x:xs)
            | newAdv > maxAdv = DrawLineFull True [DrawWord cs] : go 0 [] (x:xs)
            | otherwise       = go newAdv (cs <> [x]) xs
            where
            newAdv = a + stepAdvance spaceAdv x

    renderLines :: [DrawLineStep] -> RenderAction
    renderLines (DrawLineFull _ ws:[]) = renderWords spaceAdv $ trimWords ws
    renderLines ls    = renderComposition . go 0 $ map renderLine ls
        where
        go  _ []     = []
        go !a (x:xs)
            | a > layoutHeight = []
            | otherwise        = T.translateY (-a) x : go newAdv xs
            where
            newAdv = a + layoutLineHeight

    calcLineLength :: DrawLineStep -> AbsoluteSize
    calcLineLength (DrawLineFull _ ws) =
        pointsToPixels $ sum $ map wordAdvance $ trimWords ws

    trimWords = dropWhileEnd isDrawWordSpace . dropWhile isDrawWordSpace

    renderLine :: DrawLineStep -> RenderAction
    renderLine (DrawLineFull isFull ws) = case layoutTextAlign of
        TextAlign_Left    -> renderDef
        TextAlign_Right   -> T.translateX       leftover    renderDef
        TextAlign_Center  -> T.translateX (divf leftover 2) renderDef
        TextAlign_Justify -> if isFull
            then renderWords distrSpace wsTrimmed -- distribute spaces
            else renderDef -- draw as is
        where
        wsTrimmed  = trimWords ws
        leftover   = layoutWidth - lineLength
        lineLength = pointsToPixels $ sum $ map wordAdvance wsTrimmed
        renderDef  = renderWords spaceAdv wsTrimmed
        distrSpace = spaceAdv
                   + pixelsToPoints (divf leftover (countSpaces wsTrimmed))
        -- + pixelsToPoints (leftover / (fromIntegral $ countSpaces wsTrimmed))

        divf x y = fromIntegral (div (floor x :: Int) y)

    countSpaces = sum . map f
        where
        f (DrawWordSpace ct) = ct
        f _                  = 0

    renderWords lineSpaceAdv = renderComposition . go 0
        where
        go  _ [] = []
        go !a (x:xs) = case x of
            DrawWordSpace ct -> go (a + ct*lineSpaceAdv) xs
            DrawWord ws      -> renderWord a ws <> go (a + wordAdvance x) xs

    renderWord ia = go ia
        where
        go  _ [] = []
        go !a (x:xs) = case x of
            DrawRichChar d    -> drawChar a d : go (a + d^.advance) xs
            DrawRichImg t adv -> drawTex  a t : go (a + adv) xs
            _                 -> go (a + stepAdvance spaceAdv x) xs

        drawChar a = toRenderChar . over modelTransform (trans a)
        trans    a = T.translateX $ pointsToPixels a

        drawTex a t = renderFromAtlas $ def
            & textureId       .~ Just t
            & modelTransform  .~ T.translationX (pointsToPixels a)


stepAdvance :: Advance -> DrawRichStep -> Advance
stepAdvance spaceAdv = \case
   DrawRichChar  d      -> d^.advance
   DrawRichImg _ a      -> a
   DrawRichSpace        -> spaceAdv
   DrawRichInitialSpace -> spaceAdv
   DrawRichNewline      -> 0

toRenderChar :: DrawCharStep -> RenderAction
toRenderChar x = renderFromAtlas $ def
    & textureId       .~ (Just $ x^.texture)
    & color           .~ (x^.color)
    & modelTransform  .~ (x^.modelTransform)

{-
makeRenderChar :: FontStyle -> Char -> Engine us RenderAction
makeRenderChar fs char = do
    mDraw <- makeDrawCharStep fs char
    return $ renderComposition $ map toRenderChar $ maybeToList mDraw
-}

fullyUpdateAtlas :: Engine us ()
fullyUpdateAtlas = Atlas.fullUpdate =<< use (graphics.textureAtlas)

swapBuffers :: Engine us ()
swapBuffers = Context.swapBuffers =<< use (graphics.context)

positionToViewMatrix :: V2 Float -> Mat4
positionToViewMatrix
    = mkMatHomo2 . flip T.translate mempty . negate . fmap realToFrac

orthoProjection
    :: OrthoProjectionOpts
    -> Engine us Mat4
orthoProjection opts = do
    canvasSize <- Context.getFramebufferSize =<< use (graphics.context)
    let wh = over each fromIntegral canvasSize
    return $ orthoProjectionFor (uncurry Size wh) opts

orthoProjectionFor
    :: Size Float
    -> OrthoProjectionOpts
    -> Mat4
orthoProjectionFor (Size sw sh) opts
    = mkOrtho (hori aa) (vert bb)
    where
    wh = over each realToFrac (sw, sh)
    (aa, bb) = fromMaybe wh (orthoNorm wh <$> opts^.normalization)
    mkOrtho (a0, a1) (b0, b1) = ortho a0 a1 b0 b1 0 1

    orthoNorm (w, h) n = case n of
        OrthoNorm_Width  -> (  1, h/w)
        OrthoNorm_Height -> (w/h,   1)
        OrthoNorm_Both   -> (  1,   1)

    hori x = case opts^.boxAlign.horizontal of
        Align_Left   -> ( 0  , a  )
        Align_Center -> (-a/2, a/2)
        Align_Right  -> (-a  , 0  )
        where a = x / opts^.scale

    vert x = case opts^.boxAlign.vertical of
        Align_Top    -> ( 0  , b  )
        Align_Middle -> (-b/2, b/2)
        Align_Bottom -> (-b  , 0  )
        where b = x / opts^.scale

--------------------------------------------------------------------------------

updateZIndex :: (Word32 -> Word32) -> RenderAction -> RenderAction
updateZIndex f = \case
    RenderFromAtlas     ad -> RenderFromAtlas     $ over zindex f ad
    RenderComposition t rs -> RenderComposition t $ map (updateZIndex f) rs
    RenderSimpleText  d tx -> RenderSimpleText (over zindex f d) tx

setZIndex :: Word32 -> RenderAction -> RenderAction
setZIndex = updateZIndex . const

setZIndexAtLeast :: Word32 -> RenderAction -> RenderAction
setZIndexAtLeast = updateZIndex . max

setZIndexAtMost :: Word32 -> RenderAction -> RenderAction
setZIndexAtMost = updateZIndex . min

--------------------------------------------------------------------------------

renderShape :: ShapeDesc -> RenderAction
renderShape d = RenderFromAtlas $ def
    & color          .~ (d^.color)
    & modelTransform .~ (d^.modelTransform <> sca)
    & radius         .~ rad
    & zindex         .~ (d^.zindex)
    where
    sca = T.scaling (if d^.shapeType == SimpleCircle then 2 else 1 :: Float)
    rad = case d^.shapeType of
        SimpleSquare -> 2
        SimpleCircle -> 1

renderFromAtlas :: AtlasDesc -> RenderAction
renderFromAtlas = RenderFromAtlas

-- renderTexture :: TextureDesc -> RenderAction
-- renderTexture = RenderTexture

renderComposition :: [RenderAction] -> RenderAction
renderComposition = RenderComposition mempty

renderActionBBox :: RenderAction -> Maybe (BBox Float)
renderActionBBox x = viaNonEmpty bboxUnion
    $ mapMaybe (uncurry transformBBox) $ go mempty x []
    where
    unitBBox = BBox (-0.5) 0.5
    transformBBox t
        = viaNonEmpty bboxFromList . T.transform t . rectToList . bboxToRect
    go tt = \case
        RenderFromAtlas     ad -> ((tt <> ad^.modelTransform, unitBBox):)
        RenderComposition t rs -> foldr (.) id $ map (go (tt<>t)) rs
        RenderSimpleText  _ _  -> id -- TODO: calc bbox

renderImg :: Img -> RenderAction
renderImg i = renderFromAtlas $ def
    & colorMix  .~ (i^.colorMix)
    & color     .~ (i^.color)
    & textureId .~ Just (i^.texture)
    & part      .~ (i^.part)
    & zindex    .~ (i^.zindex)
    & T.scaleX (realToFrac sw) -- (i^.size.width)  * realToFrac (i^.part.width))
    & T.scaleY (realToFrac sh) -- (i^.size.height) * realToFrac (i^.part.height))
    where
    sw = fromMaybe (i^.size.width)  $ i^?part.traverse.width
    sh = fromMaybe (i^.size.height) $ i^?part.traverse.height

renderSimpleText :: SimpleTextDesc -> Text -> RenderAction
renderSimpleText = RenderSimpleText

setDefaultFonts :: [FontFamilyName] -> FontSize -> Engine us ()
setDefaultFonts fns fs = do
    graphics.defaultFontStyle .= Just def
    graphics.defaultFontStyle._Just.fonts    .= fns
    graphics.defaultFontStyle._Just.fontSize .= fs

