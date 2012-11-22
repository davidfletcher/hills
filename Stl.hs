module Stl ( Tri, R3, toStl ) where

import Numeric

type R3 = (Double, Double, Double)

type Tri = (R3, R3, R3)

toStl :: String -> [Tri] -> String
toStl name tris =
    unlines ( ["solid " ++ name]
              ++ concatMap facet tris
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
