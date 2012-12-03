module Area ( Area
            , areaSize
            , areaSW
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , secsPerSamp
            )
where

import LatLong

type SampCount = Int

data Area = Area { areaSW :: LatLong, areaSize :: (SampCount, SampCount) }
            deriving Show

areaFromCentreAndSize :: LatLong -> (SampCount, SampCount) -> Maybe Area
areaFromCentreAndSize cent (latSz, longSz) = do
  sw <- latLongFromSecs ( centLat - halfLatSz * secsPerSamp,
                          centLong - halfLongSz * secsPerSamp )
  return $ Area { areaSW = sw, areaSize = (latSz, longSz) }
    where (centLat, centLong) = latLongToSecs cent
          halfLatSz = (latSz + 1) `quot` 2
          halfLongSz = (longSz + 1) `quot` 2

areaFromSouthwestAndSize :: LatLong -> (SampCount, SampCount) -> Area
areaFromSouthwestAndSize sw size = Area { areaSW = sw, areaSize = size }

secsPerSamp :: Int
secsPerSamp = 3
