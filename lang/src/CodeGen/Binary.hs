{-# LANGUAGE FlexibleInstances #-}
module CodeGen.Binary where

import           CodeGen.ASM
import           Data.Binary          (Binary, put, get, encodeFile, encode, Put)
import           Data.Binary.Bits.Put (putBool, runBitPut, BitPut)
import           Data.Word            (Word8)
import           Control.Monad        ((>>))
import           CodeGen.Vectors      (Chunk(..))
import qualified Data.ByteString.Lazy            as BS

genBinary :: FilePath -> [ASMInstruction] -> ConstStorage -> IO ()
genBinary path asm strg = BS.writeFile path (encode asm `BS.append` encode strg)

instance {-# OVERLAPPING #-} Binary [ASMInstruction] where
  put xs = mapM_ put xs >> put (255 :: Word8)
  get    = undefined

instance Binary ASMInstruction where
  put Add              = runBitPut $ putArray [0,1,1,1,1,1,1,0]
  put Sub              = runBitPut $ putArray [0,1,1,1,0,1,1,0]
  put Mul              = runBitPut $ putArray [0,1,1,1,1,0,1,0]
  put Div              = runBitPut $ putArray [0,1,1,1,0,0,1,0]
  put AddS             = runBitPut $ putArray [0,1,1,1,0,1,0,0]
  put SubS             = runBitPut $ putArray [0,1,1,1,1,1,0,0]
  put MulS             = runBitPut $ putArray [0,1,1,1,1,0,0,0]
  put DivS             = runBitPut $ putArray [0,1,1,1,0,0,0,0]
  put MovS1            = runBitPut $ putArray [0,1,0,1,1,0,0,0]
  put MovS2            = runBitPut $ putArray [0,1,0,1,1,0,0,1]
  put (JumpIP i)       = runBitPut $ putArray [1,0] >> put6BitAdr i
  put (JumpIPZ i)      = runBitPut $ putArray [1,1] >> put6BitAdr i
  put (JumpZ (Lab i))  = runBitPut $ putArray [1,1] >> put6BitAdr i
  put (Jump (Lab i))   = runBitPut $ putArray [1,0] >> put6BitAdr i
  put (Label lab)      = runBitPut $ putArray [0,1,1,0,0,0,0,0]
  put (Load (Addr i))  = runBitPut $ putArray [0,1,0,0,1] >> put3BitAdr i
  put (Store (Addr i)) = runBitPut $ putArray [0,1,0,1,0] >> put3BitAdr i
  put (Push (Addr i))  = runBitPut $ putArray [0,1,0,0,0] >> put3BitAdr i
  get = undefined

putArray :: [Int] -> BitPut ()
putArray = mapM_ pb
      where pb 1 = putBool $ True
            pb 0 = putBool $ False

put6BitAdr :: Int -> BitPut ()
put6BitAdr i = do
  let adr = toBin i
  let toAdd = 6 - (length adr)
  if toAdd >= 0
    then
      let fullAdr = replicate toAdd 0 ++ adr
      in putArray fullAdr
    else
      error "Address of JUMP too big"

put3BitAdr :: Int -> BitPut ()
put3BitAdr i = do
  let adr = toBin i
  let toAdd = 3 - (length adr)
  if toAdd >= 0
    then
      let fullAdr = replicate toAdd 0 ++ adr
      in putArray fullAdr
    else
      error $ "Address of Push, Load or Store too big " ++ (show toAdd) ++ " " ++ (show adr)


toBin 0 = [0]
toBin n = reverse (helper n)
  where helper 0 = []
        helper n = let (q,r) = n `divMod` 2 in r : helper q

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
  put xs = mapM_ (\x -> put x) xs >> put (255 :: Word8)
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
