{-# Language TypeFamilies #-}
module Engine.Graphics
    ( module Types
    , initGraphics
    , draw
    , drawBatch
    , swapBuffers
    , fullyUpdateAtlas

    , batchRenderAction

    , pixelsToPoints
    , pointsToPixels

    , loadFont
    , loadFontFamily
    , createFontHierarchy
    , makeRenderChar
    -- , makeRenderText
    , makeRenderTextLayout

    , renderShape
    , renderFromAtlas
    , renderTexture
    , renderComposition

    , loadTextureToAtlas
    ) where

import Delude
import Linear
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Diagrams.Transform as T
import qualified Diagrams.TwoD.Transform as T

import Engine.Graphics.Types as Types
import Engine.Graphics.Draw.Atlas
import Engine.Graphics.Draw.Shape
import Engine.Graphics.Draw.Texture

import Engine.FontsManager
import Engine.Layout.Types

import Engine.Graphics.TextureAtlas (TextureAtlas)
import qualified Engine.Graphics.TextureAtlas as Atlas
import qualified Engine.Context as Context
import Engine.Types
import Engine.Context (Context)
import Engine.Graphics.Utils (loadImageSync, createTextureBufferFrom)

initDrawProcedure :: MonadIO m => TextureAtlas -> m DrawProcedure
initDrawProcedure atlas = do
    drawAtlas    <- initDrawAtlas
    drawTextures <- initDrawTextures
    drawShapes   <- initDrawShapes
 -- drawPolygons <- initDrawPolygons
    return $ \p r -> do
        drawAtlas atlas p $ r^.requestedAtlas
        drawTextures    p $ r^.requestedTextures
        drawShapes      p $ r^.requestedShapes
     -- drawPolygons    p $ r^.requestedPolygons

batchRenderAction :: RenderAction -> DrawRequest
batchRenderAction = go def
    where
    go r = \case
        RenderFromAtlas     desc -> r & requestedAtlas    %~ (desc:)
        RenderShape         desc -> r & requestedShapes   %~ (desc:)
        RenderTexture       desc -> r & requestedTextures %~ (desc:)
        RenderComposition t acts -> foldl' go r $ T.transform t acts

--------------------------------------------------------------------------------

initGraphics :: MonadIO m => Context -> m GraphicsState
initGraphics ctx = do
    atlas <- Atlas.newAtlas
    mDPI <- Context.getPrimaryDPI
    logOnce $ "Monitor DPI: " <> show mDPI
    fm <- initFontsManager mDPI
    proc <- initDrawProcedure atlas
    return $ GraphicsState
        { _context       = ctx
        , _drawProcedure = proc
        , _textureAtlas  = atlas
        , _fontsManager  = fm
        }

draw :: Mat4 -> RenderAction -> Engine us ()
draw projMatrix = drawBatch projMatrix . batchRenderAction

drawBatch :: Mat4 -> DrawRequest -> Engine us ()
drawBatch projMatrix batch = do
    drawProc <- use $ graphics.drawProcedure
    liftIO $ drawProc projMatrix batch

loadTextureToAtlas :: FilePath -> Engine us (Maybe Texture)
loadTextureToAtlas path = do
    atlas <- use $ graphics.textureAtlas
    -- mTex <- loadTexture path
    mImg <- loadImageSync path
    case mImg of
        Nothing  -> return Nothing
        Just img -> do
            buf <- createTextureBufferFrom img
            Atlas.addTexture atlas buf
            -- deleteTextureBuffer buf
            let tex = buf ^. texture
            return (Just tex)

{-
loadTexture :: MonadIO m => FilePath -> m (Maybe Texture)
loadTexture path = do
    mImg <- loadImageSync path
    case mImg of
        Nothing  -> return Nothing
        Just img -> return . Just =<< createTexture2d img
-}

loadFont :: FontName -> FilePath -> Engine us (Maybe Font)
loadFont fontName path = do
    fm <- use $ graphics.fontsManager
    managerLoadFont fm fontName path

loadFontFamily
    :: FontFamilyName -> FontFamilyDesc -> Engine us (Maybe FontFamily)
loadFontFamily famName desc = do
    fm <- use $ graphics.fontsManager
    managerLoadFontFamily fm famName desc

createFontHierarchy :: [FontFamilyName] -> Engine us FontHierarchy
createFontHierarchy names = do
    fm <- use $ graphics.fontsManager
    managerCreateFontHierarchy fm names

makeDrawCharStep :: FontStyle -> Char -> Engine us (Maybe DrawCharStep)
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
-- type Pixels = Double

pointsToPixels :: Int -> Double
pointsToPixels = (/64) . fromIntegral

pixelsToPoints :: Double -> Int
pixelsToPoints = floor . (*64)

{-
makeRenderText :: FontStyle -> Text -> Engine us RenderAction
makeRenderText fs text = do
    m   <- getFontMetrics (fs^.font) (fs^.fontSize)
    dws <- mapM (toDrawCharList . toString) $ words text
    return $ vertOff m $ renderComposition $ concat $ go m 0 dws
    where
    vertOff m = T.translateY (pointsToPixels $ negate $ m^.verticalOffset)
    toDrawCharList
        = fmap catMaybes . mapM (makeDrawCharStep fs)

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
-}

data DrawRichStep
   = DrawRichChar  DrawCharStep
   | DrawRichImg   Texture Advance
   | DrawRichSpace
   | DrawRichInitialSpace
   | DrawRichNewline

makeRenderTextLayout :: TextLayout -> Engine us RenderTextLayout
makeRenderTextLayout t = do
    ms <- mapM (uncurry getFontMetrics) styles
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
    uniq = ordNub . mapMaybe (\f -> (,f^.fontSize) <$> getPF f)
    getPF f = getPrimaryFont (f^.fonts) (f^.bold) (f^.italic)
    getStyle = \case
        RichText_Span fs _ -> Just fs
        RichText_Image   _ -> Nothing

    -- toRichSteps :: [RichText] -> [DrawRichStep]
    toRichSteps = fmap concat . mapM toDraw
    -- toRichSteps _ = []

    -- toDraw :: RichText -> [DrawRichStep]
    toDraw :: RichText -> Engine us [DrawRichStep]
    toDraw = \case
        RichText_Span  fs x -> catMaybes <$> (mapM (toDrawChar fs) $ toString x)
        RichText_Image img  -> return [DrawRichImg (img^.texture) imgAdv]
            where
            imgAdv = pixelsToPoints $ img^.size._x -- TODO: fix this!

    -- toDrawChar :: FontStyle -> Char -> DrawRichStep
    toDrawChar fs c
        | c == '\n'        = return $ Just DrawRichNewline
        | cat Char.Space   = return $ Just DrawRichSpace
        | cat Char.Control = return Nothing
        | otherwise        = fmap DrawRichChar <$> makeDrawCharStep fs c
        where
        cat = (Char.generalCategory c ==)

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
            & textureId      .~ t
            & modelTransform .~ T.translationX (pointsToPixels a)


stepAdvance :: Advance -> DrawRichStep -> Advance
stepAdvance spaceAdv = \case
   DrawRichChar  d      -> d^.advance
   DrawRichImg _ a      -> a
   DrawRichSpace        -> spaceAdv
   DrawRichInitialSpace -> spaceAdv
   DrawRichNewline      -> 0

toRenderChar :: DrawCharStep -> RenderAction
toRenderChar x = renderFromAtlas $ def
    & textureId      .~ (x^.texture)
    & color          .~ (x^.color)
    & modelTransform .~ (x^.modelTransform)

makeRenderChar :: FontStyle -> Char -> Engine us RenderAction
makeRenderChar fs char = do
    mDraw <- makeDrawCharStep fs char
    return $ renderComposition $ map toRenderChar $ maybeToList mDraw

fullyUpdateAtlas :: Engine us ()
fullyUpdateAtlas = Atlas.fullUpdate =<< use (graphics.textureAtlas)

swapBuffers :: Engine us ()
swapBuffers = Context.swapBuffers =<< use (graphics.context)

--------------------------------------------------------------------------------

renderShape :: ShapeDesc -> RenderAction
renderShape = RenderShape

renderFromAtlas :: AtlasDesc -> RenderAction
renderFromAtlas = RenderFromAtlas

renderTexture :: TextureDesc -> RenderAction
renderTexture = RenderTexture

renderComposition :: [RenderAction] -> RenderAction
renderComposition = RenderComposition mempty
