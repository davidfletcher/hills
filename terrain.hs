module Main where

import Data.Array.Unboxed

import Stl
import Topo

main :: IO ()
main = do
  let region = FileRegion { regFirstLine = 4450, regNumLines = 200 }
  topo <- parseFile region "srtm_36_01.asc"
  -- writePGM "out.pgm" topo
  let hs = topoHeights (4450, 1280) (200, 200) topo
  writeFile "out.stl" (toStl "topo" (model hs))

model :: Heights -> [Tri]
model hs = surface hs ++ walls hs ++ base hs

surface :: Heights -> [Tri]
surface = concatMap quadToTri .  quads

walls :: Heights -> [Tri]
walls hs = concatMap wall [nEdge, eEdge, sEdge, wEdge]
    -- we arrange the orders so they are clockwise
    -- so the triangles we make will be clockwise
    where nEdge = [hs ! (miny, x) | x <- [maxx,maxx-1..minx]]
          sEdge = [hs ! (maxy, x) | x <- [minx..maxx]]
          eEdge = [hs ! (y, maxx) | y <- [maxy,maxy-1..miny]]
          wEdge = [hs ! (y, minx) | y <- [miny..maxy]]
          ((miny, minx), (maxy, maxx)) = bounds hs

base :: Heights -> [Tri]
base hs = quadToTri ( withZ baseZ nw,
                      withZ baseZ sw,
                      withZ baseZ se,
                      withZ baseZ ne )
    where nw = hs ! (miny, minx)
          ne = hs ! (miny, maxx)
          sw = hs ! (maxy, minx)
          se = hs ! (maxy, maxx)
          ((miny, minx), (maxy, maxx)) = bounds hs

baseZ :: Double
baseZ = -100 -- TODO

wall :: [R3] -> [Tri]
wall edge = concatMap quadToTri $ zipWith wallQuad edge (tail edge)
    where wallQuad top0 top1 = (top0, top1, bottom1, bottom0)
              where bottom0 = withZ baseZ top0
                    bottom1 = withZ baseZ top1

withZ :: Double -> R3 -> R3
withZ z (x, y, _) = (x, y, z)

-- not necessarily planar
type Quad = (R3, R3, R3, R3)

quads :: Heights -> [Quad]
quads hs = map quad neCoords
    where neCoords = [(y, x) | y <- [miny..maxy-1], x <- [minx..maxx-1]]
          ((miny, minx), (maxy, maxx)) = bounds hs
          quad (y, x) = ( (pt x y),
                          (pt x (y+1)),
                          (pt (x+1) (y+1)),
                          (pt (x+1) y) )
          pt x y = hs ! (y, x)

quadToTri :: Quad -> [Tri]
quadToTri (a, b, c, d) = [(a, b, c), (c, d, a)]
