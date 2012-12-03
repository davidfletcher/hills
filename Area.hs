module Area ( Area
            , areaSize
            , areaSW
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , Arcsec
            , ArcsecSize
            )
where

import LatLong

type Arcsec = Int
type ArcsecSize = (Arcsec, Arcsec)

data Area = Area { areaSW :: LatLong
                 , areaSize :: ArcsecSize }
            deriving Show

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
