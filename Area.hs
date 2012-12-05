module Area ( Area
            , areaSize
            , areaSW
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , areaIntersect
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
  return $ Area { areaSW = sw, areaSize = (latSec, longSec) }
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
areaIntersect x y | xAllInY = Just x
                  | yAllInX = Just y
                  | xSWInY = Just $ fromSWAndNE (areaSW x) (areaNE y)
                  | ySWInX = Just $ fromSWAndNE (areaSW y) (areaNE x)
                  | otherwise = Nothing
    where xSWInY = areaContains y (areaSW x)
          ySWInX = areaContains x (areaSW y)
          xNEInY = areaContains y (areaNE x)
          yNEInX = areaContains x (areaNE y)
          xAllInY = xSWInY && xNEInY
          yAllInX = ySWInX && yNEInX

areaContains :: Area -> LatLong -> Bool
areaContains a pos = lat >= s && lat < n && long >= w && long < e
    where (lat, long) = latLongToSecs pos
          (s, w) = latLongToSecs (areaSW a)
          (n, e) = latLongToSecs (areaNE a)
