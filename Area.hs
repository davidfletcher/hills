module Area ( Area
            , areaSize
            , areaSW
            , areaNE
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , areaIntersect
            , areaSubtract
            , areaContains
            , expandToGrid
            , areaShowUser
            , Arcsec
            , ArcsecSize
            )
where

import LatLong

import Data.Maybe

type Arcsec = Int
type ArcsecSize = (Arcsec, Arcsec)

data Area = Area { areaSW :: LatLong
                 , areaSize :: ArcsecSize }
            deriving Show

areaShowUser :: Area -> String
areaShowUser a = latLongShowUser (areaSW a)
                 ++ " to "
                 ++ latLongShowUser (areaNE a)

-- TODO check areas at creation to get rid of the error here?
areaNE :: Area -> LatLong
areaNE a = fromMaybe (error "areaNE: out of range")
           (latLongFromSecs neSecs)
    where neSecs = (s + latSize, w + longSize)
          (latSize, longSize) = areaSize a
          (s, w) = latLongToSecs (areaSW a)

areaFromCentreAndSize :: LatLong -> ArcsecSize -> Maybe Area
areaFromCentreAndSize cent (latSec, longSec) = do
  sw <- latLongFromSecs ( centLat - halfLatSec, centLong - halfLongSec )
  return Area { areaSW = sw, areaSize = (latSec, longSec) }
    where (centLat, centLong) = latLongToSecs cent
          halfLatSec = (latSec + 1) `quot` 2
          halfLongSec = (longSec + 1) `quot` 2

areaFromSouthwestAndSize :: LatLong -> ArcsecSize -> Area
areaFromSouthwestAndSize sw sz =
    Area { areaSW = sw, areaSize = sz }

-- TODO won't work across the 180 degree line
fromSWAndNE :: LatLong -> LatLong -> Area
fromSWAndNE sw ne | latSize < 0 || longSize < 0
                      = error "fromSWAndNE: sw wrong side of ne"
                  | otherwise
                      = areaFromSouthwestAndSize sw (latSize, longSize)
    where (latSize, longSize) = (neLatSec - swLatSec, neLongSec - swLongSec)
          (neLatSec, neLongSec) = latLongToSecs ne
          (swLatSec, swLongSec) = latLongToSecs sw

areaIntersect :: Area -> Area -> Maybe Area
areaIntersect x y = fst (areaPartition x y)

areaSubtract :: Area -> Area -> [Area]
areaSubtract x y = snd (areaPartition y x)

areaPartition :: Area -> Area -> (Maybe Area, [Area])
areaPartition x y = (inX, notInX)
    where
      inX = maybeArea maxSW minNE
      notInX =
          case inX of
            Nothing -> [y]
            Just _ -> catMaybes [
                       maybeArea (ys, yw) (xs, xw),
                       maybeArea (ys, xw) (xs, xe),
                       maybeArea (ys, xe) (xs, ye),
                       maybeArea (xs, xe) (xn, ye),
                       maybeArea (xn, xe) (yn, ye),
                       maybeArea (xn, xw) (yn, xe),
                       maybeArea (xn, yw) (yn, xw),
                       maybeArea (xs, yw) (xn, xw) ]
      (xs, xw) = latLongToSecs (areaSW x)
      (xn, xe) = latLongToSecs (areaNE x)
      (ys, yw) = latLongToSecs (areaSW y)
      (yn, ye) = latLongToSecs (areaNE y)
      maxSW = (max xs ys, max xw yw)
      minNE = (min xn yn, min xe ye)

maybeArea :: (Arcsec, Arcsec) -> (Arcsec, Arcsec) -> Maybe Area
maybeArea (s, w) (n, e) | s < n && w < e = Just $ fromSWAndNE sw ne
                        | otherwise = Nothing
    where sw = fromMaybe (error "maybeArea") . latLongFromSecs $ (s, w)
          ne = fromMaybe (error "maybeArea") . latLongFromSecs $ (n, e)

areaContains :: Area -> LatLong -> Bool
areaContains a pos = lat >= s && lat < n && long >= w && long < e
    where (lat, long) = latLongToSecs pos
          (s, w) = latLongToSecs (areaSW a)
          (n, e) = latLongToSecs (areaNE a)

expandToGrid :: ArcsecSize -> Area -> Area
expandToGrid (latSep, longSep) area = fromSWAndNE sw' ne'
    where s' = toMultipleBelow latSep s
          n' = toMultipleAbove latSep n
          w' = toMultipleBelow longSep w
          e' = toMultipleAbove longSep e
          sw' = fromMaybe (error "expandToGrid went out of range") -- TODO
                $ latLongFromSecs (s', w')
          ne' = fromMaybe (error "expandToGrid went out of range") -- TODO
                $ latLongFromSecs (n', e')
          (s, w) = latLongToSecs (areaSW area)
          (n, e) = latLongToSecs (areaNE area)
          toMultipleBelow m x = (x `div` m) * m
          toMultipleAbove m x = ((x + m - 1) `div` m) * m
