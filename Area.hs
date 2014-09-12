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
            )
where

import LatLong

import Data.Maybe

data Area = Area { areaSW :: LatLong
                 , areaSize :: LatLongSize }
            deriving Show

areaShowUser :: Area -> String
areaShowUser a = latLongShowUser (areaSW a)
                 ++ " to "
                 ++ latLongShowUser (areaNE a)

-- TODO check areas at creation to get rid of the error here?
areaNE :: Area -> LatLong
areaNE a = fromMaybe (error "areaNE: out of range") (areaSW a `addSize` areaSize a)

areaFromCentreAndSize :: LatLong -> LatLongSize -> Maybe Area
areaFromCentreAndSize cent size = do
  sw <- cent `subSize` halfSize
  return Area { areaSW = sw, areaSize = size }
    where halfSize = latLongSizeFromSecs (halfLatSec, halfLongSec)
          halfLatSec = (latSize + 1) `quot` 2
          halfLongSec = (longSize + 1) `quot` 2
          (latSize, longSize) = latLongSizeToSecs size

areaFromSouthwestAndSize :: LatLong -> LatLongSize -> Area
areaFromSouthwestAndSize sw sz =
    Area { areaSW = sw, areaSize = sz }

-- TODO won't work across the 180 degree line
fromSWAndNE :: LatLong -> LatLong -> Area
fromSWAndNE sw ne = areaFromSouthwestAndSize sw (sizeFromCorners sw ne)

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

type Arcsec = Int

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

expandToGrid :: LatLongSize -> Area -> Area
expandToGrid size area = fromSWAndNE sw' ne'
  where
    (latSep, longSep) = latLongSizeToSecs size
    s' = toMultipleBelow latSep s
    n' = toMultipleAbove latSep n
    w' = toMultipleBelow longSep w
    e' = toMultipleAbove longSep e
    sw' = fromMaybe (error "expandToGrid went out of range") -- TODO
                    (latLongFromSecs (s', w'))
    ne' = fromMaybe (error "expandToGrid went out of range") -- TODO
                    (latLongFromSecs (n', e'))
    (s, w) = latLongToSecs (areaSW area)
    (n, e) = latLongToSecs (areaNE area)
    toMultipleBelow m x = (x `div` m) * m
    toMultipleAbove m x = ((x + m - 1) `div` m) * m
