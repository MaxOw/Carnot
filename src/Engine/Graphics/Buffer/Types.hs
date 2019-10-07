{-# Language ExistentialQuantification #-}
{-# Language ConstraintKinds #-}
{-# Language StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Engine.Graphics.Buffer.Types where

import Delude
import Linear
import Graphics.GL
import Foreign.Storable.Generic
import qualified Data.Vector.Storable as Storable

import Engine.Backend.Types

data AttribDesc = AttribDesc
   { field_location :: GLuint
   , field_enum     :: GLenum
   , field_size     :: GLint
   , field_count    :: GLint
   } deriving (Generic, Show)

type AttribOffset = GLint
type StructSize   = GLint

data ArrayBuffer a = ArrayBuffer
   { field_buffer     :: Buffer
   , field_structSize :: StructSize
   , field_structDesc :: [AttribDesc]
   } deriving (Generic)

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
   { field_texCoord      :: V3 Float
   , field_color         :: V4 Float
   , field_modelX        :: V3 Float
   , field_modelY        :: V3 Float
   , field_radius        :: Float
   , field_colorMix      :: Float
   , field_customPageNum :: Float
   } deriving (Generic)

type DrawBatch = Storable.Vector AtlasBatchItem

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

