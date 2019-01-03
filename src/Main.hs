{-# Language TemplateHaskell #-}
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

data St = St
   { _atlasNames :: Map Text Texture
   , _rend       :: DrawRequest -- RenderAction
   , _fontHierarchy :: FontHierarchy
   -- , _rend       :: Mat4 -> Engine St () -- RenderAction
   }
makeLenses ''St

integrate :: Integrator us
integrate _ = return ()

handleEvent :: EventHandler us
handleEvent = \case
    EventKey Key'Q _ _ _ -> closeWindow
    _                    -> return ()

render :: Renderer St
render _delta s = do
    fitViewport
    glClearColor 1 1 1 0
    glClear GL_COLOR_BUFFER_BIT

    projM <- orthoProjection
    -- draw projM $ s^.rend
    -- drawBatch projM $ s^.rend
    let hier = s^.fontHierarchy
    renderLayout <- makeRenderLayout $ testLayout hier
    draw projM renderLayout
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

testLayout :: FontHierarchy -> Layout
testLayout f = Layout_Box desc0
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

    fn = makeFontStyle f 12

testTextLayout :: FontHierarchy -> TextLayout
testTextLayout f = def
   -- & textAlign     .~ TextAlign_Justify
   & textAlign     .~ TextAlign_Left
   & size          .~ Size 620 400
   & minLineHeight .~ 20
   & content       .~ fillerText f

fillerText :: FontHierarchy -> [RichText]
fillerText f = -- mconcat $
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
    fn = makeFontStyle f 12
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

getAtlasTexture :: Engine us Texture
getAtlasTexture = do
    atlas <- use $ graphics.textureAtlas
    let pref = textureAtlas_primaryPages atlas
    ls <- readIORef pref
    let l0 = viaNonEmpty head $ toList ls
    let Just tex = fmap (view texture . atlasPage_buffer) l0
    return tex

initialize :: Engine St ()
initialize = do
    mtex0 <- loadTextureToAtlas "imgs/barrel_E.png"
    userState.atlasNames %= Map.alter (const mtex0) "barrel0"

    mtex1 <- loadTextureToAtlas "imgs/barrel_E.png"
    userState.atlasNames %= Map.alter (const mtex1) "barrel1"
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

    hier <- createFontHierarchy ["Arial", "SourceHanSerif"]
    -- hier <- createFontHierarchy ["SourceHanSerif", "Arial"]

    -- let fs = makeFontStyle hier 12
    -- rendLine <- makeRenderText fs $ "Sed ut perspiciatis"
    rendRich <- makeRenderTextLayout $ testTextLayout hier
    -- draw projM rendRich
    let rnd = renderComposition
            -- [ rendLine
            -- [ T.translateY (-40) rendRich
            [ T.translateX 10 $ rendRich^.renderAction
            , rendCircle
            , rendSquare
            -- [ rendRich
            ]
    -- userState.rend .= flip draw rnd
    userState.rend .= batchRenderAction rnd
    userState.fontHierarchy .= hier

    return ()

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

main :: IO ()
main = do
    ctx <- initWindow "Test" (800, 600)
    let initialState = St
            { _atlasNames = mempty
            -- , _rend = \_ -> return ()
            , _rend = def
            , _fontHierarchy = undefined
            }

    igniteEngine ctx initialState $ Ignition
        { initializer  = initialize
        , eventHandler = handleEvent
        , integrator   = integrate
        , renderer     = render
        }

