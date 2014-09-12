module Model (model) where

import Topo (Heights, MetresF)
import Stl

import Data.Array

model :: Double -> Heights -> Model
model baseAlt hs =
    onPlatform (R3 120 120 120) . fromTris $ allTris
        where allTris = surface hs
                        ++ walls baseAlt hs
                        ++ base baseAlt hs

surface :: Heights -> [Tri]
surface = concatMap quadToTri .  quads

walls :: Double -> Heights -> [Tri]
walls baseAlt hs = concatMap (wall baseAlt) [nEdge, eEdge, sEdge, wEdge]
    -- we arrange the orders so they are clockwise
    -- so the triangles we make will be clockwise
    where nEdge = [heightToR3 (hs ! (miny, x)) | x <- [maxx,maxx-1..minx]]
          sEdge = [heightToR3 (hs ! (maxy, x)) | x <- [minx..maxx]]
          eEdge = [heightToR3 (hs ! (y, maxx)) | y <- [maxy,maxy-1..miny]]
          wEdge = [heightToR3 (hs ! (y, minx)) | y <- [miny..maxy]]
          ((miny, minx), (maxy, maxx)) = bounds hs

wall :: Double -> [R3] -> [Tri]
wall baseAlt edge = concatMap quadToTri $ zipWith wallQuad edge (tail edge)
    where wallQuad top0 top1 = (top0, top1, bottom1, bottom0)
              where bottom0 = withZ baseAlt top0
                    bottom1 = withZ baseAlt top1

base :: Double -> Heights -> [Tri]
base alt hs = quadToTri ( withZ alt nw,
                          withZ alt sw,
                          withZ alt se,
                          withZ alt ne )
    where nw = heightToR3 (hs ! (miny, minx))
          ne = heightToR3 (hs ! (miny, maxx))
          sw = heightToR3 (hs ! (maxy, minx))
          se = heightToR3 (hs ! (maxy, maxx))
          ((miny, minx), (maxy, maxx)) = bounds hs

withZ :: Double -> R3 -> R3
withZ z (R3 x y _) = R3 x y z

-- not necessarily planar
type Quad = (R3, R3, R3, R3)

quads :: Heights -> [Quad]
quads hs = map quad swCoords
    where swCoords = [(y, x) | y <- [miny..maxy-1], x <- [minx..maxx-1]]
          ((miny, minx), (maxy, maxx)) = bounds hs
          quad (y, x) = ( heightToR3 (hs ! (y, x)),
                          heightToR3 (hs ! (y, x+1)),
                          heightToR3 (hs ! (y+1, x+1)),
                          heightToR3 (hs ! (y+1, x)) )

heightToR3 :: (MetresF, MetresF, MetresF) -> R3
heightToR3 (x, y, z) = R3 x y z

quadToTri :: Quad -> [Tri]
quadToTri (a, b, c, d) = [Tri a b c, Tri c d a]
