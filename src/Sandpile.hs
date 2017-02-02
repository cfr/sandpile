module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Control.Monad

import Codec.Picture
import Codec.Picture.Png
import qualified Data.ByteString.Lazy as L

--main = L.writeFile fn $ encodePng $ toImage s
main = print s
  where fn = "out.png"
        n = 100
        s = topple (Sandpile n (V.replicate (n*n) 4))

toImage :: Sandpile -> Image Pixel8
toImage (Sandpile n g) = Image n n $ V.convert (V.map scale g)
  where scale pix = fromIntegral $ pix * (256 `div` depth)

data Sandpile = Sandpile Int (V.Vector Int) -- Sandpile n grid: grid of size nxn
  deriving (Show, Eq)

depth = 4

--data CellCoord = CellCoord Int Int -- x y
--  deriving (Eq, Show)
type CellCoord = (Int, Int) -- x y

positionToCoord :: Sandpile -> Int -> CellCoord
positionToCoord (Sandpile n _) p = (x, y)
  where x = p `mod` n
        y = p `div` n

coord2Position (Sandpile n _) (x, y) = y*n + x

isValidSandpileCoord :: Sandpile -> CellCoord -> Bool
isValidSandpileCoord (Sandpile n _) (x, y) = x < n && y < n && x >= 0 && y >= 0

topple :: Sandpile -> Sandpile
topple s = let s' = toppleOnce s in
           if toppable s' then topple s'
                          else s'

toppleOnce :: Sandpile -> Sandpile
toppleOnce s@(Sandpile n g) = (Sandpile n) $ V.create $ do
    --let topplePositions = V.findIndices (>= depth) g
    g' <- V.unsafeThaw g
    V.forM_ (V.enumFromN 0 (n*n)) $ \p -> do
      v <- M.read g' p
      when (v >= depth) $ do
        M.unsafeModify g' (-depth+) p
        let (x,y) = p2c p
        when (y-1 >= 0) $ M.unsafeModify g' (1+) (c2p (x, y-1))
        when (x-1 >= 0) $ M.unsafeModify g' (1+) (c2p (x-1, y))
        when (x+1 < n)  $ M.unsafeModify g' (1+) (c2p (x+1, y))
        when (y+1 < n)  $ M.unsafeModify g' (1+) (c2p (x, y+1))
    return g'
  where
    p2c = positionToCoord s
    c2p = coord2Position s
    isValid = isValidSandpileCoord s

toppable :: Sandpile -> Bool
toppable (Sandpile _ g) = V.any (>= depth) g

