module Engine.Graphics.RenderAction
    ( updateZIndex, setZIndex, setZIndexAtLeast, setZIndexAtMost

    , renderShape
    , renderFromAtlas
    , renderComposition

    , renderActionBBox

    , renderImg, renderImgRaw
    ) where

import Delude
import qualified Diagrams.Transform as T
import qualified Diagrams.TwoD.Transform as T
import Engine.Common.Types
import Engine.Graphics.Types

--------------------------------------------------------------------------------

updateZIndex :: (Word32 -> Word32) -> RenderAction -> RenderAction
updateZIndex f = \case
    RenderFromAtlas     ad -> RenderFromAtlas     $ over zindex f ad
    RenderComposition t rs -> RenderComposition t $ map (updateZIndex f) rs

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

renderImg :: Img -> RenderAction
renderImg = renderFromAtlas . renderImgRaw

renderImgRaw :: Img -> AtlasDesc
renderImgRaw i = def
    & colorMix  .~ (i^.colorMix)
    & color     .~ (i^.color)
    & textureId .~ Just (i^.texture)
    & part      .~ (i^.part)
    & zindex    .~ (i^.zindex)
    & T.scaleX (realToFrac sw) -- (i^.size.width)  * realToFrac (i^.part.width))
    & T.scaleY (realToFrac sh) -- (i^.size.height) * realToFrac (i^.part.height))
    where
    sw = fromMaybe (i^.size.width)  $ i^?part.traverse.size.width
    sh = fromMaybe (i^.size.height) $ i^?part.traverse.size.height

