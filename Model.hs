module Model (model) where

import Topo (Heights)
import Stl

import Data.Array

model :: Double -> Heights -> Model
model baseAlt hs =
    onPlatform (120, 120, 120) . fromTris $ allTris
        where allTris = surface hs
                        ++ walls baseAlt hs
                        ++ base baseAlt hs

surface :: Heights -> [Tri]
surface = concatMap quadToTri .  quads

walls :: Double -> Heights -> [Tri]
walls baseAlt hs = concatMap (wall baseAlt) [nEdge, eEdge, sEdge, wEdge]
    -- we arrange the orders so they are clockwise
    -- so the triangles we make will be clockwise
    where nEdge = [hs ! (miny, x) | x <- [maxx,maxx-1..minx]]
          sEdge = [hs ! (maxy, x) | x <- [minx..maxx]]
          eEdge = [hs ! (y, maxx) | y <- [maxy,maxy-1..miny]]
          wEdge = [hs ! (y, minx) | y <- [miny..maxy]]
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
    where nw = hs ! (miny, minx)
          ne = hs ! (miny, maxx)
          sw = hs ! (maxy, minx)
          se = hs ! (maxy, maxx)
          ((miny, minx), (maxy, maxx)) = bounds hs

withZ :: Double -> R3 -> R3
withZ z (x, y, _) = (x, y, z)

-- not necessarily planar
type Quad = (R3, R3, R3, R3)

quads :: Heights -> [Quad]
quads hs = map quad swCoords
    where swCoords = [(y, x) | y <- [miny..maxy-1], x <- [minx..maxx-1]]
          ((miny, minx), (maxy, maxx)) = bounds hs
          quad (y, x) = ( hs ! (y, x),
                          hs ! (y, x+1),
                          hs ! (y+1, x+1),
                          hs ! (y+1, x) )

quadToTri :: Quad -> [Tri]
quadToTri (a, b, c, d) = [(a, b, c), (c, d, a)]
