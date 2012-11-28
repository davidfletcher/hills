module Topo ( Topo
            , mkTopo
            , Sect
            , mkSect
            , Area
            , areaSize
            , areaSW
            , areaFromCentreAndSize
            , areaFromSouthwestAndSize
            , secsPerSamp
            , topoHeights
            , Heights )
where

import LatLong

import Data.Array.Unboxed
import Data.Maybe (fromMaybe)
import Data.Word

newtype Topo = Topo { topoSects :: [Sect] }

mkTopo :: [Sect] -> Topo
mkTopo = Topo

data Sect = Sect { sectArea :: Area, sectArray :: Arr }

type MetresI = Int

-- TODO check divisions are exact
heightAt :: LatLong -> Sect -> MetresI
heightAt pos sect = sectArray sect ! (y, x)
    where (y, x) = (dLat `quot` secsPerSamp, dLong `quot` secsPerSamp)
          (dLat, dLong) = (posLat - swLat, posLong - swLong)
          (posLat, posLong) = latLongToSecs pos
          (swLat, swLong) = latLongToSecs (areaSW (sectArea sect))

type SampCount = Int

data Area = Area { areaSW :: LatLong, areaSize :: (SampCount, SampCount) }
            deriving Show

secsPerSamp :: Int
secsPerSamp = 3

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

type Arr = UArray (Int, Int) MetresI

mkSect :: Area -> [[Int]] -> Sect
mkSect area vals = Sect area arr
    where
      arr = array bnds (zip ixsNorthtoSouth (concat vals))
      ixsNorthtoSouth = [(y, x) | y <- [maxy, maxy-1 .. 0], x <- [0..maxx]]
      (szy, szx) = areaSize area
      (maxy, maxx) = (szy - 1, szx - 1)
      bnds = ((0, 0), (maxy, maxx))

type MetresF = Double
type Heights = Array (Int, Int) (MetresF, MetresF, MetresF)

-- TODO for other lat/longs
degreeLatAt56N, degreeLongAt56N :: MetresF
degreeLatAt56N = 111360
degreeLongAt56N = 60772
mPerLatSampAt56N, mPerLongSampAt56N :: MetresF
mPerLatSampAt56N = fromIntegral secsPerSamp * degreeLatAt56N / 3600
mPerLongSampAt56N = fromIntegral secsPerSamp * degreeLongAt56N / 3600

topoHeights :: Area -> Topo -> Heights
topoHeights area (Topo sects) = arrayFromFn bnds pointAt
    where
      sect = head sects -- TODO
      bnds = ((0, 0), (szLat - 1, szLong - 1))
      (szLat, szLong) = areaSize area
      (south, west) = latLongToSecs (areaSW area)
      pointAt (y, x) = (dblX, dblY, dblZ)
          where
            dblX = fromIntegral x * mPerLongSampAt56N
            dblY = fromIntegral y * mPerLatSampAt56N
            dblZ = fromIntegral (heightAt ll sect)
            ll = fromMaybe (error "lat/long out of range")
                 (latLongFromSecs (south + y * secsPerSamp, west + x * secsPerSamp))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))
