module Area ( Area
            , areaArcsecSize
            , areaSW
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , secsPerSamp
            , Arcsec
            , ArcsecSize
            )
where

import LatLong

type Arcsec = Int
type ArcsecSize = (Arcsec, Arcsec)
type SampCount = Int

data Area = Area { areaSW :: LatLong, areaSize :: (SampCount, SampCount) }
            deriving Show

areaArcsecSize :: Area -> ArcsecSize
areaArcsecSize a = (szy * secsPerSamp, szx * secsPerSamp)
    where (szy, szx) = areaSize a

areaFromCentreAndSize :: LatLong -> ArcsecSize -> Maybe Area
areaFromCentreAndSize cent (latSec, longSec) = do
  sw <- latLongFromSecs ( centLat - halfLatSz * secsPerSamp,
                          centLong - halfLongSz * secsPerSamp )
  return $ Area { areaSW = sw, areaSize = (latSz, longSz) }
    where (centLat, centLong) = latLongToSecs cent
          halfLatSz = (latSz + 1) `quot` 2
          halfLongSz = (longSz + 1) `quot` 2
          (latSz, longSz) = (latSec `quot` secsPerSamp,
                             longSec `quot` secsPerSamp)

areaFromSouthwestAndSize :: LatLong -> ArcsecSize -> Area
areaFromSouthwestAndSize sw (szy, szx) =
    Area { areaSW = sw
         , areaSize = (szy `quot` secsPerSamp, szx `quot` secsPerSamp) }

secsPerSamp :: Int
secsPerSamp = 3
