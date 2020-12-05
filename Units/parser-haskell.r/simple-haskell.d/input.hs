module Cards where

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
newtype Fd = Fd CInt
data Bool 
  = True | False
type Name = String


getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref, 
                    haBuffers=spare_ref,
                    haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
          buf  <- readIORef ref
          case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)


{-|
  This is a docstring
-}
add :: Integer -> Integer -> Integer
add x y  = x + y
