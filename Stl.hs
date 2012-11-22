module Stl ( Model, Tri, R3, toString, fromTris, onPlatform ) where

import Data.List
import Numeric

newtype Model = Model { modelTris :: [Tri] }

type Tri = (R3, R3, R3)

type R3 = (Double, Double, Double)

fromTris :: [Tri] -> Model
fromTris = Model

toString :: String -> Model -> String
toString name model =
    unlines ( ["solid " ++ name]
              ++ concatMap facet (modelTris model)
              ++ ["endsolid " ++ name] )

facet :: Tri -> [String]
facet (p0, p1, p2) = [
 "  facet normal " ++ stlPoint (0, 0, 0),
 "  outer loop",
 "    vertex " ++ stlPoint p0,
 "    vertex " ++ stlPoint p1,
 "    vertex " ++ stlPoint p2,
 "  endloop",
 "  endfacet" ]

stlPoint :: R3 -> String
stlPoint (x, y, z) = s x ++ " " ++ s y ++ " " ++ s z
    where s x = (showEFloat Nothing x) ""

translate :: R3 -> Model -> Model
translate offset = Model . map (applyToPoints (addR3 offset)) . modelTris

scale :: R3 -> Model -> Model
scale scales = Model . map (applyToPoints (scaleR3 scales)) . modelTris

applyToPoints :: (R3 -> R3) -> Tri -> Tri
applyToPoints f (p0, p1, p2) = ( f p0, f p1, f p2 )

binR3 :: (Double -> Double -> Double) -> R3 -> R3 -> R3
binR3 f (x0, y0, z0) (x1, y1, z1) = x2 `seq` y2 `seq` z2 `seq` (x2, y2, z2)
    where (x2, y2, z2) = (f x0 x1, f y0 y1, f z0 z1)

addR3 :: R3 -> R3 -> R3
addR3 = binR3 (+)

scaleR3 :: R3 -> R3 -> R3
scaleR3 = binR3 (*)

negateR3 :: R3 -> R3
negateR3 (x, y, z) = (negate x, negate y, negate z)

toPositiveOctant :: Model -> Model
toPositiveOctant m = let minp = minPoint m in
                     translate (negateR3 minp) m

minPoint :: Model -> R3
minPoint = foldl1' (binR3 min) . concatMap triPoints . modelTris

maxPoint :: Model -> R3
maxPoint = foldl1' (binR3 max) . concatMap triPoints . modelTris

dimensions :: Model -> R3
dimensions m = (maxx - minx, maxy - miny, maxz - minz)
    where (minx, miny, minz) = minPoint m
          (maxx, maxy, maxz) = maxPoint m

triPoints :: Tri -> [R3]
triPoints (p0, p1, p2) = [p0, p1, p2]

fitToBox :: R3 -> Model -> Model
fitToBox (boxx, boxy, boxz) m = scale (smallest, smallest, smallest) m'
    where smallest = minimum [sx, sy, sz]
          (dx, dy, dz) = dimensions m'
          (sx, sy, sz) = (boxx / dx, boxy / dy, boxz / dz)
          m' = toPositiveOctant m

centerXY :: Model -> Model
centerXY m = translate (-dx / 2, -dy / 2, 0) m
    where (dx, dy, _) = dimensions m

onPlatform :: R3 -> Model -> Model
onPlatform box = centerXY . fitToBox box
