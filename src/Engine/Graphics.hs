{-# Language TypeFamilies #-}
module Engine.Graphics
    ( module Types
    -- , initDrawProcedure
    , initGraphics
    , showWindow
    , draw
    , drawOpts
    , drawRequest
    , getBatchAndPrepIO
    , getDrawBatchOptsIO
    , getPrepBatchIO
    , drawBatch
    , fitViewport
    , clearColorWhite, clearColorBlack
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
    , makeRenderTextLine
    , makeRenderText

    , updateZIndex, setZIndex, setZIndexAtLeast, setZIndexAtMost

    , renderShape
    , renderFromAtlas
    , renderComposition
    , setDefaultFonts

    , renderActionBBox

    , loadTextureToAtlas
    , addImageToAtlas
    , renderImg, renderImgRaw
    ) where

import Delude
import Linear
-- import qualified Data.Sequence as Seq
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
-- import qualified Data.Vector.Algorithms.Intro as M
-- import qualified Data.Vector.Mutable as M
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW
import Data.Hashable

import Diagrams.Core (InSpace, Transformable)
import qualified Diagrams.Transform as T
import qualified Diagrams.TwoD.Transform as T

import Engine.Graphics.Types as Types
-- import Engine.Graphics.Draw.Atlas
import Engine.Graphics.Draw.Atlas1

import Engine.FontsManager
import Engine.Layout.Types

import Engine.Graphics.RenderAction
import qualified Engine.Graphics.TextureAtlas as Atlas
import qualified Engine.Context as Context
import Engine.Types
import Engine.Context (Context)
import Engine.Graphics.Utils
    -- (loadImageSync, createTextureBufferFrom, mkMatHomo2)
import Codec.Picture (DynamicImage, Image(..), convertRGBA8)

import qualified Data.GrowVector as G

import qualified Engine.Graphics.TaskManager as TaskManager
import qualified Engine.Graphics.TextureCache as TextureCache
import qualified Engine.Graphics.DrawBatchCache as DrawBatchCache

--------------------------------------------------------------------------------

initGraphics :: MonadIO m => Context -> m GraphicsState
initGraphics ctx = do
    tm <- TaskManager.new
    atlas <- Atlas.new tm
    tch <- TextureCache.newCache tm atlas
    dbc <- DrawBatchCache.new
    mDPI <- Context.getPrimaryDPI
    -- logOnce $ "Monitor DPI: " <> show mDPI
    fm <- initFontsManager mDPI
    (setupAtlas, drawAtlas) <- initDrawAtlas atlas
    return $ GraphicsState
        { _context          = ctx
        , _setupProcedure   = setupAtlas
        , _drawProcedure    = drawAtlas
        , _textureAtlas     = atlas
        , _fontsManager     = fm
        , _defaultFontStyle = Nothing
        , _textCache        = tch
        , _drawBatchCache   = dbc
        , _taskManager      = tm
        }

--------------------------------------------------------------------------------

{-
initDrawProcedure :: MonadIO m => TextureAtlas -> m DrawProcedure
initDrawProcedure atlas = do
    (setupAtlas, drawAtlas) <- initDrawAtlas atlas
    return $ \p r -> do
        drawAtlas p =<< setupAtlas (r^.requestedAtlas)
     -- unlessEmpty (r^.requestedTextures) $ drawTextures    p
     -- drawShapes      p $ r^.requestedShapes
     -- drawPolygons    p $ r^.requestedPolygons

    where
    -- unlessEmpty x f
        -- | isn't _Empty x = f x
        -- | otherwise      = return ()
-}

batchRenderAction :: MonadIO m => RenderAction -> m DrawRequest
batchRenderAction ra = do
    gv <- liftIO G.new
    go gv ra
    liftIO $ G.unsafeToVector gv
    where
    go gv = \case
        RenderFromAtlas   d    -> liftIO $ G.snoc gv d
        RenderComposition t rs -> mapM_ (go gv) (T.transform t rs)
        -- RenderComposition t rs -> mapM_ (go gv) rs -- (T.transform t rs)


getBatchAndPrepIO :: Engine us (RenderAction -> IO DrawBatch)
getBatchAndPrepIO = do
    setupProc <- use $ graphics.setupProcedure
    return (setupProc <=< batchRenderAction)

getDrawBatchOptsIO :: Engine us (Bool -> Mat4 -> DrawBatch -> IO ())
getDrawBatchOptsIO = use $ graphics.drawProcedure

showWindow :: MonadIO m => Context -> m ()
showWindow = liftIO . GLFW.showWindow

draw :: Mat4 -> RenderAction -> Engine us ()
draw projMatrix = drawRequest projMatrix <=< batchRenderAction

drawOpts :: Bool -> Mat4 -> RenderAction -> Engine us ()
drawOpts m p = drawBatchOpts m p <=< prepBatch <=< batchRenderAction

drawRequest :: Mat4 -> DrawRequest -> Engine us ()
drawRequest projMatrix = drawBatch projMatrix <=< prepBatch

prepBatch :: DrawRequest -> Engine us DrawBatch
prepBatch req = do
    setupProc <- use $ graphics.setupProcedure
    liftIO $ setupProc req

getPrepBatchIO :: Engine us (DrawRequest -> IO DrawBatch)
getPrepBatchIO = use $ graphics.setupProcedure

drawBatch :: Mat4 -> DrawBatch -> Engine us ()
drawBatch projMatrix batch = do
    drawProc <- use $ graphics.drawProcedure
    liftIO $ drawProc True projMatrix batch

drawBatchOpts :: Bool -> Mat4 -> DrawBatch -> Engine us ()
drawBatchOpts mixColors projMatrix batch = do
    drawProc <- use $ graphics.drawProcedure
    liftIO $ drawProc mixColors projMatrix batch

loadTextureToAtlas :: FilePath -> Engine us (Maybe Img)
loadTextureToAtlas path = do
    mImg <- loadImageSync path
    case mImg of
        Nothing  -> return Nothing
        Just img -> Just <$> addImageToAtlas img

addImageToAtlas :: DynamicImage -> Engine us Img
addImageToAtlas img = do
    atlas <- use $ graphics.textureAtlas
    buf <- createTextureBufferFrom img
    Atlas.addTexture atlas buf
    let (Image w h _) = convertRGBA8 img
    let tex = buf^.texture
    return $ mkImg tex (Size w h)

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
        { field_texture        = glyph^.buffer.texture
        , field_color          = fs^.color
        , field_modelTransform = makeTrans glyph
        , field_advance        = glyph^.advance
        }

-- type Points = Int
-- type Pixels = Float

pointsToPixels :: Int -> Float
pointsToPixels = (/64) . fromIntegral

pixelsToPoints :: Float -> Int
pixelsToPoints = floor . (*64)

makeRenderText :: BoxAlign -> FontStyle -> Text -> Engine us RenderAction
makeRenderText align fs text = snd <$> makeRenderTextLine True align fs text

makeRenderTextLine
    :: Bool -> BoxAlign -> FontStyle -> Text
    -> Engine us (TextLineDesc, RenderAction)
makeRenderTextLine withCache align fs text = do
    fh  <- retriveHierarchy fs
    case getPrimaryFont (fh^.fonts) (fs^.bold) (fs^.italic) of
        Just f -> do
            m   <- getFontMetrics f (fs^.fontSize)
            dws <- mapM (toDrawCharList fh . toString) $ words text
            let s = fmap pointsToPixels $ Size (calcWidth m dws) (m^.lineHeight)
            let shouldCache = Text.length text > 1 && withCache
            let renderText
                     = transformBoxAlign s align $ vertOff m
                     $ renderComposition $ concat $ go shouldCache m 0 dws
            let spaceAdv = pointsToPixels $ view minSpaceAdvance m
            let desc = def & size            .~ s
                           & verticalOffset  .~ vertOffPx m
                           & minSpaceAdvance .~ spaceAdv

            let col = fs^.color
            tch <- use $ graphics.textCache
            let ts = floor <$> s
            let key = hash (align, hashFontStyle fs, text)
            rend <- if shouldCache
                then cacheText tch ts key (Just col) renderText
                -- then return renderText
                else return renderText
            return (desc, rend)
        Nothing -> return (def, mempty)
    where
    cacheText tch ts key mcol ra = do
        batchAsyncIO <- getBatchAndPrepIO
        drawBatchOptsIO <- getDrawBatchOptsIO
        TextureCache.retriveOrAdd tch ts key mcol (batchAsyncIO ra)
            $ drawBatchOptsIO False
    vertOffPx m = pointsToPixels $ m^.verticalOffset
    vertOff m = T.translateY (negate $ vertOffPx m)
    toDrawCharList fh = fmap catMaybes . mapM (makeDrawCharStep fh)

    go _ _ _       []  = []
    go c m !adv (w:ws) = drawWord c adv w : go c m (adv + off) ws
        where
        off = sumOf (traverse.advance) w + view minSpaceAdvance m

    drawWord _ _       []  = []
    drawWord c !adv (l:ls) = drawChar l : drawWord c (adv + off) ls
        where
        drawChar = toRenderChar (not c) . over modelTransform trans
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
        { field_size         = outputSize
        , field_renderAction = vertOff vertoffset renderLines
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
            imgAdv = pixelsToPoints $ fromIntegral $ img^.size.width

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
    return $ f { field_fonts = h }

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
        _      -> Size layoutWidth (fromIntegral plen * layoutLineHeight)

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
        pointsToPixels $ sum $ map wordAdvance $ trimWords ws

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
        lineLength = pointsToPixels $ sum $ map wordAdvance wsTrimmed
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

        drawChar a = toRenderChar True . over modelTransform (trans a)
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

toRenderChar :: Bool -> DrawCharStep -> RenderAction
toRenderChar shouldApplyColor x = renderFromAtlas $ def
    & textureId       .~ (Just $ x^.texture)
    & applyColor
    & modelTransform  .~ (x^.modelTransform)
    where
    applyColor = if shouldApplyColor then set color (x^.color) else id
    -- & color           .~ (x^.color)
    -- & colorMix        .~ 0

{-
makeRenderChar :: FontStyle -> Char -> Engine us RenderAction
makeRenderChar fs char = do
    mDraw <- makeDrawCharStep fs char
    return $ renderComposition $ map toRenderChar $ maybeToList mDraw
-}

fullyUpdateAtlas :: Engine us ()
fullyUpdateAtlas = Atlas.fullUpdate =<< use (graphics.textureAtlas)

fitViewport :: Engine us ()
fitViewport = do
    (w, h) <- Context.getFramebufferSize =<< use (graphics.context)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)

clearColorWhite :: Engine us ()
clearColorWhite = do
    glClearColor 1 1 1 0
    glClear GL_COLOR_BUFFER_BIT

clearColorBlack :: Engine us ()
clearColorBlack = do
    glClearColor 0 0 0 0
    glClear GL_COLOR_BUFFER_BIT

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

--------------------------------------------------------------------------------

setDefaultFonts :: [FontFamilyName] -> FontSize -> Engine us ()
setDefaultFonts fns fs = do
    graphics.defaultFontStyle .= Just def
    graphics.defaultFontStyle._Just.fonts    .= fns
    graphics.defaultFontStyle._Just.fontSize .= fs

