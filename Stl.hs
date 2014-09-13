module Stl ( Model
           , Tri(..)
           , R3(..)
           , toBinary
           , toAscii
           , fromTris
           , translate
           , scale
           , toPositiveOctant )
where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import Data.List
import Data.Monoid
import GHC.Float (double2Float)
import Numeric (showFFloat)

newtype Model = Model { modelTris :: [Tri] }

data Tri = Tri !R3 !R3 !R3

data R3 = R3 !Double !Double !Double

fromTris :: [Tri] -> Model
fromTris = Model

toBinary :: Model -> L.ByteString
toBinary model =
  B.toLazyByteString ( header <> numTris <> tris )
  where header = mconcat (replicate 80 (B.word8 0))
        numTris = B.int32LE . fromIntegral . length . modelTris $ model
        tris = mconcat . map binTri . modelTris $ model

binTri :: Tri -> B.Builder
binTri (Tri p0 p1 p2) =
  binPoint normal
  <> binPoint p0
  <> binPoint p1
  <> binPoint p2
  <> attrByteCount
  where
    normal = R3 0 0 0
    attrByteCount = B.word16LE 0

binPoint :: R3 -> B.Builder
binPoint (R3 x y z) = b x <> b y <> b z
  where b = B.floatLE . double2Float

toAscii :: String -> Model -> L.ByteString
toAscii name model =
  B.toLazyByteString (
    B.stringUtf8 "solid " <> B.stringUtf8 name <> newline
    <> mconcat (map ascTri (modelTris model))
    <> B.stringUtf8 "endsolid " <> B.stringUtf8 name <> newline )

newline :: B.Builder
newline = B.charUtf8 '\n'

space :: B.Builder
space = B.charUtf8 ' '

ascTri :: Tri -> B.Builder
ascTri (Tri p0 p1 p2) =
     B.stringUtf8 "  facet normal 0 0 0" <> newline
  <> B.stringUtf8 "  outer loop" <> newline
  <> B.stringUtf8 "    vertex " <> stlPoint p0 <> newline
  <> B.stringUtf8 "    vertex " <> stlPoint p1 <> newline
  <> B.stringUtf8 "    vertex " <> stlPoint p2 <> newline
  <> B.stringUtf8 "  endloop" <> newline
  <> B.stringUtf8 "  endfacet" <> newline

stlPoint :: R3 -> B.Builder
stlPoint (R3 x y z) = d x <> space <> d y <> space <> d z
  where d n = B.stringUtf8 (showFFloat (Just 6) n [])

translate :: R3 -> Model -> Model
translate offset = Model . map (applyToPoints (addR3 offset)) . modelTris

scale :: R3 -> Model -> Model
scale scales = Model . map (applyToPoints (scaleR3 scales)) . modelTris

applyToPoints :: (R3 -> R3) -> Tri -> Tri
applyToPoints f (Tri p0 p1 p2) = Tri (f p0) (f p1) (f p2)

minR3 :: R3 -> R3 -> R3
minR3 (R3 x0 y0 z0) (R3 x1 y1 z1) = R3 (min x0 x1) (min y0 y1) (min z0 z1)

maxR3 :: R3 -> R3 -> R3
maxR3 (R3 x0 y0 z0) (R3 x1 y1 z1) = R3 (max x0 x1) (max y0 y1) (max z0 z1)

addR3 :: R3 -> R3 -> R3
addR3 (R3 x0 y0 z0) (R3 x1 y1 z1) = R3 (x0 + x1) (y0 + y1) (z0 + z1)

scaleR3 :: R3 -> R3 -> R3
scaleR3 (R3 x0 y0 z0) (R3 x1 y1 z1) = R3 (x0 * x1) (y0 * y1) (z0 * z1)

negateR3 :: R3 -> R3
negateR3 (R3 x y z) = R3 (negate x) (negate y) (negate z)

toPositiveOctant :: Model -> Model
toPositiveOctant m = let minp = minPoint m in
                     translate (negateR3 minp) m

minPoint :: Model -> R3
minPoint = foldl1' minR3 . concatMap triPoints . modelTris

maxPoint :: Model -> R3
maxPoint = foldl1' maxR3 . concatMap triPoints . modelTris

dimensions :: Model -> R3
dimensions m = R3 (maxx - minx) (maxy - miny) (maxz - minz)
    where R3 minx miny minz = minPoint m
          R3 maxx maxy maxz = maxPoint m

triPoints :: Tri -> [R3]
triPoints (Tri p0 p1 p2) = [p0, p1, p2]

fitToBox :: R3 -> Model -> Model
fitToBox (R3 boxx boxy boxz) m = scale (R3 smallest smallest smallest) m'
    where smallest = minimum [sx, sy, sz]
          (R3 dx dy dz) = dimensions m'
          (sx, sy, sz) = (boxx / dx, boxy / dy, boxz / dz)
          m' = toPositiveOctant m

centerXY :: Model -> Model
centerXY m = translate (R3 (-dx / 2) (-dy / 2) 0) m
    where (R3 dx dy _) = dimensions m
