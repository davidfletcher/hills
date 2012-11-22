module Main where

import Data.Array.Unboxed

import Stl
import Topo

main :: IO ()
main = do
  let region = FileRegion { regFirstLine = 4300, regNumLines = 100 }
  topo <- parseFile region "srtm_36_01.asc"
  writePGM "out.pgm" topo
  let hs = topoHeights (4300, 500) (100, 100) topo
  writeFile "out.stl" (toStl "topo" (surface hs))

surface :: Heights -> [Tri]
surface = concatMap quadToTri .  quads

-- not necessarily planar
type Quad = (R3, R3, R3, R3)

quads :: Heights -> [Quad]
quads hs = map quad bottomLeftCoords
    where bottomLeftCoords = [(y, x) | y <- [miny..maxy-1], x <- [minx..maxx-1]]
          ((miny, minx), (maxy, maxx)) = bounds hs
          quad (y, x) = ( (pt x y),
                          (pt (x+1) y),
                          (pt (x+1) (y+1)),
                          (pt x (y+1)) )
          pt x y = hs ! (y, x)

quadToTri :: Quad -> [Tri]
quadToTri (a, b, c, d) = [(a, b, c), (c, d, a)]
