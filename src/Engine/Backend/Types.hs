module Engine.Backend.Types where

import Delude
import Foreign (Ptr)
import Graphics.GL (GLuint, GLint, GLsizeiptr)
import Engine.Common.Types

--------------------------------------------------------------------------------

type Program = GLuint
type Buffer  = GLuint
type VAO     = GLuint
type Texture = GLuint
type Shader  = GLuint
type Framebuffer     = GLuint
type UniformLocation = GLint
type AttribLocation  = GLint

type BufferData = (GLsizeiptr, Ptr ())

--------------------------------------------------------------------------------

noTexture :: Texture
noTexture = 0


data TextureBuffer = TextureBuffer
   { field_framebuffer :: Framebuffer
   , field_texture     :: Texture
   , field_width       :: GLint
   , field_height      :: GLint
   } deriving (Generic)
instance HasWidth  TextureBuffer GLint
instance HasHeight TextureBuffer GLint
instance HasSize   TextureBuffer (Size Int32) where
    size f b = fmap ss (f bb)
        where
        bb = Size (b^.width) (b^.height)
        ss = \(Size w h) -> b & set width w & set height h

{-
instance HasField' "field_size" TextureBuffer (V2 GLint) where
    field' f b = fmap ss (f bb)
        where
        bb = V2 (b^.width) (b^.height)
        ss = \(V2 w h) -> b & width  .~ w & height .~ h
-}

