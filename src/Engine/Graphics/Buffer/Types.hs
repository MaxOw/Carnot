{-# Language TemplateHaskell #-}
{-# Language ExistentialQuantification #-}
{-# Language ConstraintKinds #-}
{-# Language StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Engine.Graphics.Buffer.Types where

import Delude
import Linear
import Graphics.GL
import Foreign.Storable.Generic

import Engine.Backend.Types

data AttribDesc = AttribDesc
   { attribDesc_location :: GLuint
   , attribDesc_enum     :: GLenum
   , attribDesc_size     :: GLint
   , attribDesc_count    :: GLint
   } deriving Show
makeFieldsCustom ''AttribDesc

type AttribOffset = GLint
type StructSize   = GLint

data ArrayBuffer a = ArrayBuffer
   { arrayBuffer_buffer     :: Buffer
   , arrayBuffer_structSize :: StructSize
   , arrayBuffer_structDesc :: [AttribDesc]
   }
makeFieldsCustom ''ArrayBuffer

--------------------------------------------------------------------------------

type ArrayBufferDesc = [(String, (GLenum, GLint))]

instance ArrayBufferItemEnum Float where baseType_enum _ = GL_FLOAT
instance ArrayBufferItemCount Float where baseType_count _ = 1

instance ArrayBufferItemEnum x => ArrayBufferItemEnum (V2 x) where
    baseType_enum = baseType_enum . fromProxy
instance ArrayBufferItemEnum x => ArrayBufferItemEnum (V3 x) where
    baseType_enum = baseType_enum . fromProxy
instance ArrayBufferItemEnum x => ArrayBufferItemEnum (V4 x) where
    baseType_enum = baseType_enum . fromProxy

instance ArrayBufferItemCount (V2 x) where baseType_count _ = 2
instance ArrayBufferItemCount (V3 x) where baseType_count _ = 3
instance ArrayBufferItemCount (V4 x) where baseType_count _ = 4

instance (Storable a) => GStorable (V2 a) where
    gsizeOf = sizeOf
    galignment = alignment
    gpeekByteOff = peekByteOff
    gpokeByteOff = pokeByteOff

instance (Storable a) => GStorable (V3 a) where
    gsizeOf = sizeOf
    galignment = alignment
    gpeekByteOff = peekByteOff
    gpokeByteOff = pokeByteOff

instance (Storable a) => GStorable (V4 a) where
    gsizeOf = sizeOf
    galignment = alignment
    gpeekByteOff = peekByteOff
    gpokeByteOff = pokeByteOff

--------------------------------------------------------------------------------

type ArrayBufferField x =
    (ArrayBufferItemEnum x, ArrayBufferItemCount x, Storable x)

data AttribDescProxy = forall x . ArrayBufferField x
  => Attrib String (Proxy x)

class ArrayBufferItemEnum x where baseType_enum :: proxy x -> GLenum
class ArrayBufferItemCount x where baseType_count :: proxy x -> GLint

class Storable x => ArrayBufferItem x where
    arrayBufferItemDesc :: proxy x -> [AttribDescProxy]

data AtlasBatchItem = AtlasBatchItem
   { atlasBatchItem_texCoord      :: V3 Float
   , atlasBatchItem_color         :: V4 Float
   , atlasBatchItem_modelX        :: V3 Float
   , atlasBatchItem_modelY        :: V3 Float
   , atlasBatchItem_radius        :: Float
   , atlasBatchItem_colorMix      :: Float
   , atlasBatchItem_customPageNum :: Float
   } deriving (Generic)

instance GStorable AtlasBatchItem

instance ArrayBufferItem AtlasBatchItem where
    arrayBufferItemDesc _ =
        [ Attrib "TexCoord"      (Proxy @(V3 Float))
        , Attrib "Color"         (Proxy @(V4 Float))
        , Attrib "ModelX"        (Proxy @(V3 Float))
        , Attrib "ModelY"        (Proxy @(V3 Float))
        , Attrib "Radius"        (Proxy @Float)
        , Attrib "ColorMix"      (Proxy @Float)
        , Attrib "CustomPageNum" (Proxy @Float)
        ]

fromProxy :: proxy (f x) -> Proxy x
fromProxy _ = Proxy

