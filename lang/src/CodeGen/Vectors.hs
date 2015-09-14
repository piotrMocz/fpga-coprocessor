module CodeGen.Vectors where

import Control.Lens

data Chunk  = Chunk { _len :: Int, _body :: (Int, Int, Int, Int, Int, Int, Int, Int) } deriving (Show, Eq, Ord)
type Vector = [Chunk]

zero :: Chunk
zero = Chunk 0 (0, 0, 0, 0, 0, 0, 0, 0)

scalar :: Int -> Chunk
scalar x = Chunk 1 (0, 0, 0, 0, 0, 0, 0, x)


packOne :: [Int] -> Chunk
packOne []                = Chunk 0 (0, 0, 0, 0, 0, 0, 0, 0)
packOne [a]               = Chunk 1 (0, 0, 0, 0, 0, 0, 0, a)
packOne [a,b]             = Chunk 2 (0, 0, 0, 0, 0, 0, b, a)
packOne [a,b,c]           = Chunk 3 (0, 0, 0, 0, 0, c, b, a)
packOne [a,b,c,d]         = Chunk 4 (0, 0, 0, 0, d, c, b, a)
packOne [a,b,c,d,e]       = Chunk 5 (0, 0, 0, e, d, c, b, a)
packOne [a,b,c,d,e,f]     = Chunk 6 (0, 0, f, e, d, c, b, a)
packOne [a,b,c,d,e,f,g]   = Chunk 7 (0, g, f, e, d, c, b, a)
packOne [a,b,c,d,e,f,g,h] = Chunk 8 (h, g, f, e, d, c, b, a)
packOne _                 = error "PackOne can only handle lists up to 8 elements long"

pack' :: [Int] -> Vector -> Vector
pack' (a:b:c:d:e:f:g:h:rest) vs = pack' rest (Chunk 8 (h,g,f,e,d,c,b,a) : vs)
pack' is vs = (packOne is) : vs


pack :: [Int] -> Vector
pack is = pack' (reverse is) []


chunkCnt :: Int -> Int
chunkCnt x = (x `div` 8) + (if x `mod` 8 == 0 then 0 else 1)