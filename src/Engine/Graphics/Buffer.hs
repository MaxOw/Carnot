{-# Language ConstraintKinds #-}
{-# Language ScopedTypeVariables #-}
module Engine.Graphics.Buffer (module Engine.Graphics.Buffer) where

import Delude
import Foreign.Ptr (plusPtr, nullPtr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Graphics.GL
import Engine.Graphics.Utils
import Data.Proxy (asProxyTypeOf)

import Engine.Graphics.Buffer.Types as Engine.Graphics.Buffer

--------------------------------------------------------------------------------

createArrayBuffer :: forall m x. (MonadIO m, ArrayBufferItem x)
    => Program -> m (ArrayBuffer x)
createArrayBuffer program = do
    buf <- glCreateBuffer
    let p = Proxy :: Proxy x
    let ds = arrayBufferItemDesc p
    ss <- mapM (makeAttribDesc program) ds
    return $ ArrayBuffer
        { field_buffer     = buf
        , field_structSize = fromIntegral $ proxySizeOf p
        , field_structDesc = ss
        }

setArrayBufferData :: (Storable x, MonadIO m) => ArrayBuffer x -> Vector x -> m ()
setArrayBufferData b v = do
    glBindBuffer GL_ARRAY_BUFFER (b^.buffer)
    let s = fromIntegral $ Vector.length v * sizeOf (Vector.head v)
    liftIO $ Vector.unsafeWith v $ \ptr ->
        glBufferData GL_ARRAY_BUFFER s (castPtr ptr) GL_STATIC_DRAW
    glBindBuffer GL_ARRAY_BUFFER noBuffer

bindArrayBufferPointers :: MonadIO m => ArrayBuffer x -> m ()
bindArrayBufferPointers b = do
    bindBufferStruct (b^.buffer) (b^.structSize) (b^.structDesc)

--------------------------------------------------------------------------------

bindBufferStruct
    :: MonadIO m => Buffer -> StructSize -> [AttribDesc] -> m ()
bindBufferStruct buf ssize xs = do
    glBindBuffer GL_ARRAY_BUFFER buf
    -- mapM_ (glEnableVertexAttribArray . view location) xs
    go 0 xs
    where
    go _ [] = return ()
    go off (a:as) = do
        newOffset <- setVertexAttribPointer a ssize off
        go newOffset as

setVertexAttribPointer
    :: MonadIO m => AttribDesc -> StructSize -> AttribOffset -> m AttribOffset
setVertexAttribPointer (AttribDesc loc aEnum aSize ct) fullSize off = do
    let offsetPtr = plusPtr nullPtr $ fromIntegral off
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc ct aEnum GL_FALSE fullSize offsetPtr
    let newOffset = off + aSize
    return newOffset

makeAttribDesc :: MonadIO m => Program -> AttribDescProxy -> m AttribDesc
makeAttribDesc program (Attrib n p) = do
    loc <- getAttribLocation program n
    return $ AttribDesc
        { field_location = fromIntegral loc
        , field_enum     = baseType_enum p
        , field_size     = fromIntegral $ proxySizeOf p
        , field_count    = baseType_count p
        }

proxySizeOf :: Storable x => proxy x -> Int
proxySizeOf = sizeOf . asProxyTypeOf (error "This shouldn't happen")

{-
bindArrayBufferDesc :: MonadIO m => Program -> ArrayBufferDesc x -> m ()
bindArrayBufferDesc program = go 0 . unArrayBufferDesc
    where
    structSize = sizeOf (undefined :: x)
    go  _ [] = return ()
    go !n (a:as) = do
        loc <- getAttribLocation program (a^.name)
        glVertexAttribPointer
            (fromIntegral loc)
            (a^.elemCount)
            (a^.typeEnum)
            GL_FALSE
            structSize
            (nullGLPtr + n)
        let off = x^.elemCount * x^.elemSize
        go (n + off) as
-}

