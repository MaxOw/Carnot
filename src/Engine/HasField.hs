{-# Language CPP #-}
{-# Language DefaultSignatures #-}
{-# Language MonoLocalBinds #-}
module Engine.HasField (module Engine.HasField) where

import Control.Lens (Lens')
import Data.Generics.Product as Engine.HasField (HasField'(field'))

class HasSize s a | s -> a where
    size :: Lens' s a
    default size :: HasField' "field_size" s a => Lens' s a
    size = field' @"field_size"

class HasWidth s a | s -> a where
    width :: Lens' s a
    default width :: HasField' "field_width" s a => Lens' s a
    width = field' @"field_width"

class HasHeight s a | s -> a where
    height :: Lens' s a
    default height :: HasField' "field_height" s a => Lens' s a
    height = field' @"field_height"

#define MakeField2(X,FX) X :: HasField' "FX" s a => Lens' s a; X = field' @"FX"
#define MakeField(X) MakeField2(X,field_/**/X)

MakeField(direction)
MakeField(justify)
MakeField(textAlign)
MakeField(boxAlign)
MakeField(minLineHeight)
MakeField(modelTransform)
MakeField(texture)
-- MakeField(size)
MakeField(offset)
MakeField(ftFace)
-- MakeField(width)
-- MakeField(height)
MakeField(framebuffer)
MakeField(page)
MakeField(primaryPages)
MakeField(tasks)
MakeField(atlasMap)
MakeField(buffer)
MakeField(maxTextureSize)
MakeField(slots)
MakeField(customPages)
MakeField(customPage)
MakeField(quadTree)
MakeField(topSlotSize)
MakeField(structDesc)
MakeField(structSize)
MakeField(radius)
MakeField(colorMix)
MakeField(textureId)
MakeField(part)
MakeField(color)
MakeField(fontsMap)
MakeField(familiesMap)
MakeField(fontBase)
MakeField(hierarchy)
MakeField(fontItalic)
MakeField(fontBold)
MakeField(fontBoldItalic)
MakeField(hierarchiesMap)
MakeField(deviceDPI)
MakeField(ftLib)
MakeField(keyItalic)
MakeField(keyBold)
MakeField(keyChar)
MakeField(keyFontSize)
MakeField(glyphsMap)
MakeField(advance)
MakeField(maxBucketSize)
MakeField(maxCellSize)
MakeField(minCellSize)
MakeField(center)
MakeField(fontSize)
MakeField(fonts)
MakeField(shapeType)
MakeField(italic)
MakeField(normalization)
MakeField(bold)
MakeField(content)
MakeField(minSpaceAdvance)
MakeField(verticalOffset)
MakeField(lineHeight)
MakeField(fontName)
MakeField(bearing)
MakeField(scale)
MakeField(zindex)
MakeField(valid)
MakeField(targetBuffer)
MakeField(sourceBuffer)
MakeField(position)
MakeField(bufferMargin)
MakeField(drawScale)
MakeField(atlasCustomPageId)
MakeField(left)
MakeField(right)
MakeField(bottom)
MakeField(top)
MakeField(border)
MakeField(padding)
MakeField(renderAction)
