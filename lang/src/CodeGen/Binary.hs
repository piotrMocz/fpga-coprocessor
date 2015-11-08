{-# LANGUAGE FlexibleInstances #-}
module CodeGen.Binary where

import           CodeGen.ASM
import           Data.Binary     (Binary, put, get, encodeFile, encode)
import           Data.Word       (Word8)
import           Control.Monad   ((>>))
import           CodeGen.Vectors (Chunk(..))
import qualified Data.ByteString.Lazy            as BS

genBinary :: FilePath -> [ASMInstruction] -> ConstStorage -> IO ()
genBinary path asm strg = BS.writeFile path (encode asm `BS.append` encode strg)

instance {-# OVERLAPPING #-} Binary [ASMInstruction] where
  put xs = mapM_ put xs >> put (255 :: Word8)
  get    = undefined

instance Binary ASMInstruction where
  put Add                    = put (0 :: Word8)
  put Sub                    = put (1 :: Word8)
  put Mul                    = put (2 :: Word8)
  put Div                    = put (3 :: Word8)
  put Dup                    = put (4 :: Word8)
  put AddS                   = put (5 :: Word8)
  put SubS                   = put (6 :: Word8)
  put MulS                   = put (7 :: Word8)
  put DivS                   = put (8 :: Word8)
  put MovS1                  = put (9 :: Word8)
  put MovS2                  = put (10 :: Word8)
  put (JumpIP i)             = put (11 :: Word8) >> put (fromIntegral i    :: Word8)
  put (JumpIPZ i)            = put (12 :: Word8) >> put (fromIntegral i    :: Word8)
  put (JumpZ lab)            = put (13 :: Word8) >> put lab
  put (Jump lab)             = put (14 :: Word8) >> put lab
  put (Label lab)            = put (15 :: Word8) >> put lab
  put (Load addr size)       = put (16 :: Word8) >> put addr
                                                 >> put (fromIntegral size :: Word8)
  put (Store addr size)      = put (17 :: Word8) >> put addr
                                                 >> put (fromIntegral size :: Word8)
  put (Push addr)            = put (18 :: Word8) >> put addr
  get = undefined

instance Binary Addr where
  put (Addr i) = put (fromIntegral i :: Word8)
  get = undefined

instance Binary Lab where
  put (Lab i) = put (fromIntegral i :: Word8)
  get = undefined

instance Binary Chunk where
  put (Chunk len bd) = put (fromIntegral len  :: Word8)
                       >> putTuple bd
      where putTuple (a,b,c,d,e,f,g,h) =  put (fromIntegral a :: Word8)
                                       >> put (fromIntegral b :: Word8)
                                       >> put (fromIntegral c :: Word8)
                                       >> put (fromIntegral d :: Word8)
                                       >> put (fromIntegral e :: Word8)
                                       >> put (fromIntegral f :: Word8)
                                       >> put (fromIntegral g :: Word8)
                                       >> put (fromIntegral h :: Word8)
  get = undefined

instance {-# OVERLAPPING #-} Binary [ConstData] where
  put xs = mapM_ (\x -> put x >> put (255 :: Word8)) xs
  get = undefined


instance Binary ConstData where
  put (ConstScalar i) = put (fromIntegral i :: Word8)
  put (ConstVector (a,b,c,d,e,f,g,h)) = put (fromIntegral a :: Word8)
                                     >> put (fromIntegral b :: Word8)
                                     >> put (fromIntegral c :: Word8)
                                     >> put (fromIntegral d :: Word8)
                                     >> put (fromIntegral e :: Word8)
                                     >> put (fromIntegral f :: Word8)
                                     >> put (fromIntegral g :: Word8)
                                     >> put (fromIntegral h :: Word8)
  get = undefined
