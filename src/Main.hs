{-# Options_GHC -fno-warn-unused-imports #-}
module Main where

import Delude
import Graphics.GL
import qualified Data.Map.Strict as Map
import qualified Data.Colour as Color
import qualified Data.Colour.Names as Color

-- import qualified Diagrams.Transform as T
import qualified Diagrams.TwoD.Transform as T

import Engine
import Engine.Layout.Types
import Engine.Layout.Render
import Linear
import Tests
import Benchmarks

import Reload.Utils (reacquire)


data St = St
   { _atlasNames :: Map Text Img
   -- , _rend       :: DrawRequest -- RenderAction
   -- , _fontHierarchy :: FontHierarchy
   -- , _rend       :: Mat4 -> Engine St () -- RenderAction
   , _scro       :: Scroller
   , _vpos       :: V2 Float
   }
makeLenses ''St

integrate :: Integrator us
integrate _ = return ()

handleEvent :: EventHandler St -- us
handleEvent _ = return ()
{-
handleEvent = \case
    EventKey Key'J _ _ _ -> userState.vpos._y -= 1
    EventKey Key'K _ _ _ -> userState.vpos._y += 1
    EventKey Key'H _ _ _ -> userState.vpos._x -= 1
    EventKey Key'L _ _ _ -> userState.vpos._x += 1
    EventKey Key'Q _ _ _ -> closeWindow
    _                    -> return ()
    -}

render :: Renderer St
render _delta s = do
    let mtex0 = Map.lookup "barrel0" $ s^.atlasNames
    let renderBarrel = maybe mempty renderImg mtex0

    -- let vp = s^.vpos
    -- (w, h) <- getFramebufferSize =<< use (graphics.context)
    -- let vs = pure 700
    -- let vs = Size (fromIntegral w) (fromIntegral h)

    -- let viewScale = 7

    {-
    projM <- orthoProjection $ def & scale .~ 1/3 -- (1/32) -- viewScale
    let viewM = positionToViewMatrix (viewScale *^ vp)
    let viewProjM = projM !*! viewM -}
    projM <- orthoProjection $ def -- & scale .~ 1/3 -- (1/32) -- viewScale
        -- & set scale 64

    {-
    updateScroller (s^.scro) (realToFrac viewScale) vp vs $ \_ ->
        pure $ T.scale 0.1 $ T.translate (V2 30 150) $ renderBarrel
        -- T.scale (1/32) renderBarrel

    -}
    fitViewport
    -- glClearColor 0 0 0 0
    glClearColor 1 1 1 0
    glClear GL_COLOR_BUFFER_BIT


    -- let r = renderImg mtex0

    -- drawBatch projM $ s^.rend
    -- let hier = s^.fontHierarchy
    -- hier <- createFontHierarchy ["Arial", "SourceHanSerif"]
    -- setDefaultFonts ["Arial", "SourceHanSerif"] 10

    renderLayout <- makeRenderLayout testLayout
    draw projM renderLayout
    -- let dt = def & fontName .~ Just "Arial"
           -- & fontSize .~ Just 8
    draw projM $ renderComposition
        [ T.translateY (140) $ renderBarrel
        , renderSimpleText (def & set color (Color.opaque Color.red)) "Test"
        ]
    {-
    renderScroller <- makeRenderScroller (s^.scro)
    draw viewProjM $ renderComposition
        [ renderScroller
        , renderShape $ def
            & shapeType .~ SimpleSquare
            & color .~ Color.withOpacity Color.gray 0.1
            & T.scaleX (realToFrac $ vs^.width)
            & T.scaleY (realToFrac $ vs^.height)
            & T.translate (fmap realToFrac $ viewScale *^ vp)
        ]
        -}

    {-
    draw projM $ renderSimpleBox $ def
        & size  .~ Size 300 200
        & color .~ Color.opaque Color.gray
        & border.each.width   .~ 2
        -- & border.each.color   .~ Color.opaque Color.black
        & border.left.color   .~ Color.opaque Color.red
        & border.bottom.color .~ Color.opaque Color.green
        & border.right.color  .~ Color.opaque Color.blue
        -}

    swapBuffers
    where
    {-
    renderImg = \case
        Nothing -> mempty
        Just im -> renderFromAtlas $ def
            & textureId .~ Just (im^.texture)
            & colorMix  .~ 0
            & T.scaleX (realToFrac $ im^.size.width)
            & T.scaleY (realToFrac $ im^.size.height)
            -}

testLayout :: Layout
testLayout = Layout_Box desc0
    -- [ Layout_Box desc1 []
    -- , Layout_Box desc2 []
    [ hori
        [ box Color.red    "This"
        , box Color.green  "gTjypTg"
        , box Color.blue   "test."
        ]
    ]
    where
    desc0 = def
        & boxAlign .~ Center
        & size .~ Size (0.8 @@ wpct) (0.8 @@ wpct)
        & border.each.width .~ 1
        & padding.each .~ 4
        {-
    desc1 = def
        & size .~ Size (200 @@ px) (120 @@ px)
        & boxAlign .~ BottomCenter
        & border.each.width .~ 1
        & border.each.color .~ Color.opaque Color.red
    desc2 = def
        & size .~ Size (60 @@ px) (120 @@ px)
        -- & boxAlign .~ BoxAlign_TopLeft
        & boxAlign .~ TopLeft
        & border.each.width .~ 1
        & border.each.color .~ Color.opaque Color.blue
        -}

    hori = Layout_Lineup hd
    hd = def
       -- & direction .~ Vertical
       & direction .~ Horizontal
       & justify   .~ SpaceBetween

    box c t = Layout_Box (mkd c) [mkTxt t, Layout_Box mkbt []]
    mkTxt t = Layout_Text txd [RichText_Span fn t]

    txd = def
        & textAlign .~ TextAlign_Right
        -- & textAlign .~ TextAlign_Left
        -- & boxAlign  .~ BottomRight
        -- & boxAlign  .~ MiddleRight
        & boxAlign  .~ Center
        -- & boxAlign  .~ TopRight

    mkbt = def
        & boxAlign .~ Center
        & size.width  .~ 1 @@ fill
        & size.height .~ 28 @@ px
        & border.top   .width .~ 1
        & border.bottom.width .~ 1
    mkd c = def
        & boxAlign .~ Center
        -- & padding.top .~ 5
        -- & size .~ Size (2290 @@ px) (1 @@ fill)
        -- & size .~ Size (0.2 @@ cPct) (1 @@ fill)
        & size .~ Size (0.3 @@ cpct) (0.2 @@ cpct)
        -- & size .~ Size (1 @@ fill) (0.2 @@ cpct)
        -- & size .~ Size (1 @@ fill) (2240 @@ px)
        & border.each.width .~ 1
        & border.each.color .~ Color.opaque c

    fn = makeFontStyle defaultFonts 12

testTextLayout :: TextLayout
testTextLayout = def
   -- & textAlign     .~ TextAlign_Justify
   & textAlign     .~ TextAlign_Left
   & size          .~ Size 620 400
   & minLineHeight .~ 20
   & content       .~ fillerText

fillerText :: [RichText]
fillerText = -- mconcat $
    [ tb "    Sed ut ", tr "perspiciatis" , tb " unde ", bd "omnis", tb" iste "
    , it "natus", tb " error " , bi "sit", tb" voluptatem "
    , tb "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae "
    -- , "ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt "
    -- , "explicabo.\n"
    , col Color.dimgrey $ textJap <> "\n"
    , col Color.saddlebrown $ textPl
    , tb textRu ]
    {--
    , "    Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut "
    , "odit aut fugit, sed quia consequuntur magni dolores eos qui ratione "
    , "voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum "
    ]]
    , "quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam "
    , "eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat "
    , "voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam "
    , "corporis suscipit laboriosam, nisi ut aliquid ex ea commodi "
    , "consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate "
    , "velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum "
    , "fugiat quo voluptas nulla pariatur?" ]
    --}
    where
    fn = makeFontStyle defaultFonts 12
    tb = RichText_Span $ fn
    tr = RichText_Span $ fn & color  .~ Color.opaque Color.red
    it = RichText_Span $ fn & italic .~ True
    bd = RichText_Span $ fn & bold   .~ True
    bi = RichText_Span $ fn & bold   .~ True & italic .~ True

    col c = RichText_Span $ fn & color .~ Color.opaque c

textJap :: Text
textJap = mconcat
 ["昼間の桜はもちろん美しいが、日が落ちてからライトアップされた桜もまた神秘的な"
 ]
 -- ,"美しさを持っている。お酒を飲みながらのお花見もいいが、花だけをじっくりと眺め"
 -- ,"るのも、また乙なものである。"]

textPl :: Text
textPl = unlines
 -- [ "Obława! Obława! Na młode wilki obława!"
 -- , "Te dzikie zapalczywe"
 -- , "W gęstym lesie wychowane!"
    [ "Krąg śniegu wydeptany! W tym kręgu plama krwawa!"
    , "Ciała wilcze kłami gończych psów szarpane!"
    ]

textRu :: Text
textRu = unlines
    [ "Я не люблю фатального исхода,"
    , "От жизни никогда не устаю."
    ]
 -- , "Я не люблю любое время года,"
 -- , "Когда веселых песен не пою."
 -- ]


fitViewport :: Engine us ()
fitViewport = do
    (w, h) <- getFramebufferSize =<< use (graphics.context)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)

{-
orthoProjection :: Engine us Mat4
orthoProjection = do
    canvasSize <- getFramebufferSize =<< use (graphics.context)
    let (w, h) = over each fromIntegral canvasSize
    -- let a = w/h -- (h*s)
    -- let b = 1 -- /s
    let a = w/2
    let b = h/2
    return $ ortho (-a)    a    (-b)    b      0 1
 -- return $ ortho   0    (a*2)   0    (b*2)   0 1
 -- return $ ortho   0    (a*2) (-b*2)  0      0 1
 -- return $ ortho (-a*2)  0    (-b*2)  0      0 1
 -- return $ ortho (-a*2)  0      0    (b*2)   0 1
-}

getAtlasTexture :: Engine us Texture
getAtlasTexture = do
    atlas <- use $ graphics.textureAtlas
    let pref = field_primaryPages atlas
    ls <- readIORef pref
    let l0 = viaNonEmpty head $ toList ls
    let Just tex = fmap (view (buffer.texture)) l0
    return tex

defaultFonts :: [FontFamilyName]
defaultFonts = ["Arial", "SourceHanSerif"]

initialize :: Engine () St
initialize = do
    mtex0 <- loadTextureToAtlas "imgs/barrel_E.png"
    -- userState.atlasNames %= Map.alter (const mtex0) "barrel0"

    mtex1 <- loadTextureToAtlas "imgs/barrel_E.png"
    -- userState.atlasNames %= Map.alter (const mtex1) "barrel1"
    let atlasIni = catMaybes $ [("barrel0",) <$> mtex0, ("barrel1",) <$> mtex1]
    fullyUpdateAtlas

    -- mFont <- loadFont "Arial" "fonts/Arial.ttf"
    -- whenJust mFont $ \baseFont -> do

    void $ loadFontFamily "SourceHanSerif" $ def
        & fontBase       .~ "fonts/SourceHanSerif-Regular.otf"

    void $ loadFontFamily "Arial" $ def
        & fontBase       .~ "fonts/Arial.ttf"
        & fontBold       .~ Just "fonts/Arial-Bold.ttf"
        & fontBoldItalic .~ Just "fonts/Arial-Bold-Italic.ttf"
        & fontItalic     .~ Just "fonts/Arial-Italic.ttf"

    setDefaultFonts ["Arial", "SourceHanSerif"] 10
    -- hier <- createFontHierarchy ["Arial", "SourceHanSerif"]
    -- hier <- createFontHierarchy ["SourceHanSerif", "Arial"]

    -- let fs = makeFontStyle hier 12
    -- rendLine <- makeRenderText fs $ "Sed ut perspiciatis"
    -- rendRich <- makeRenderTextLayout testTextLayout
    -- draw projM rendRich
    {-
    let rnd = renderComposition
            -- [ rendLine
            -- [ T.translateY (-40) rendRich
            -- [ T.translateX 10 $ rendRich^.renderAction
            [ T.translate (V2 (-320) ( 110)) $ rendRich^.renderAction
            , rendCircle
            , rendSquare
            -- [ rendRich
            ]
            -}
    -- userState.rend .= flip draw rnd
    -- userState.rend .= batchRenderAction rnd
    -- userState.fontHierarchy .= hier

    newScro <- newScroller scroConf

    return $ St
        { _atlasNames = Map.fromList atlasIni
        -- , _rend = \_ -> return ()
        -- , _rend = batchRenderAction rnd
        -- , _fontHierarchy = undefined
        , _scro = newScro
        , _vpos = 0
        }

    {-
    where
    rendCircle
        = T.translate (V2 200 (-200))
        $ T.scaleX 100 $ T.scaleY 100
        $ renderShape $ def
            & shapeType .~ SimpleCircle
            & color     .~ Color.opaque Color.red

    rendSquare
        = T.translate (V2 400 (-200))
        $ T.scaleX 100 $ T.scaleY 100
        $ renderShape $ def
            & shapeType .~ SimpleSquare
            & color     .~ Color.opaque Color.red
    -}

scroConf :: ScrollerConfig
scroConf = def
    -- & bufferSize   .~ 1024
    -- & bufferSize   .~ 256
    -- & initialRange .~ Rect (pure (-0.5)) (pure 1)

main :: IO ()
main = do
    ctx <- reacquire 0 $ initWindow "Test" (800, 600)

    igniteEngine ctx $ Ignition
        { initializer  = initialize
        , eventHandler = handleEvent
        , integrator   = integrate
        , renderer     = render
        , finalizer    = return ()
        }

