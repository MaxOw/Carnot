{-# Language TemplateHaskell #-}
module Engine.Backend.Types where

import Delude
import Linear
import Foreign (Ptr)
import Graphics.GL (GLuint, GLint, GLsizeiptr)

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

data TextureBuffer = TextureBuffer
   { textureBuffer_framebuffer :: Framebuffer
   , textureBuffer_texture     :: Texture
   , textureBuffer_width       :: GLint
   , textureBuffer_height      :: GLint
   }
makeFieldsCustom ''TextureBuffer

instance HasSize TextureBuffer (V2 GLint) where
    size f b = fmap ss (f bb)
        where
        bb = V2 (b^.width) (b^.height)
        ss = \(V2 w h) -> b & width  .~ w & height .~ h

