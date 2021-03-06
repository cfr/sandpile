module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Control.Monad

import Codec.Picture
import Codec.Picture.Png
import qualified Data.ByteString.Lazy as L

main = L.writeFile fn $ encodePng $ toImage s
  where fn = "out.png"
        n = 500
        s = topple (Sandpile n (V.replicate (n*n) 4))

toImage :: Sandpile -> Image Pixel8
toImage (Sandpile n g) = Image n n $ V.convert (V.map scale g)
  where scale pix = fromIntegral $ pix * (256 `div` depth)

data Sandpile = Sandpile Int (V.Vector Int) -- Sandpile n grid: grid of size nxn
  deriving (Show, Eq)

depth = 4

data CellCoord = CellCoord Int Int -- x y
  deriving (Eq, Show)

positionToCoord :: Sandpile -> Int -> CellCoord
positionToCoord (Sandpile n _) p = CellCoord x y
  where x = p `mod` n
        y = p `div` n

coord2Position (Sandpile n _) (CellCoord x y) = y*n + x

isValidSandpileCoord :: Sandpile -> CellCoord -> Bool
isValidSandpileCoord (Sandpile n _) (CellCoord x y) = x < n && y < n && x >= 0 && y >= 0


neighbourhood :: Sandpile -> CellCoord -> [CellCoord]
neighbourhood s (CellCoord x y) = filter isValid neighbs
  where neighbs = [                 cc ( x ) (y-1)
                  , cc (x-1) ( y ),                 cc (x+1) ( y )
                  ,                 cc ( x ) (y+1)                 ]
        cc = CellCoord
        isValid = isValidSandpileCoord s

topple :: Sandpile -> Sandpile
topple s = let s' = toppleOnce s in
           if toppable s' then topple s'
                          else s'

toppleOnce :: Sandpile -> Sandpile
toppleOnce s@(Sandpile n g) = (Sandpile n) $ V.create $ do
    let toppleCoords = map p2c $ V.toList $ V.findIndices (>= depth) g
    g' <- V.thaw g
    forM_ toppleCoords $ \c -> do
      M.modify g' (-depth+) (c2p c)
      forM_ (neighbourhood s c) $ \c -> M.modify g' (1+) (c2p c)
    return g'
  where
    p2c = positionToCoord s
    c2p = coord2Position s
    isValid = isValidSandpileCoord s

toppable :: Sandpile -> Bool
toppable (Sandpile _ g) = V.any (>= depth) g

