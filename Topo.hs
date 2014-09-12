module Topo ( Topo
            , mkTopo
            , Sect
            , mkSect
            , topoHeights
            , Heights
            , MetresF
            )
where

import Area
import LatLong

import Control.Monad
import Control.Monad.Trans.Except
import Data.Array.Unboxed
import Data.Maybe (fromMaybe)

newtype Topo = Topo [Sect]

mkTopo :: [Sect] -> Topo
mkTopo = Topo

topoSep :: Topo -> LatLongSize
topoSep (Topo []) = latLongSizeFromSecs (1, 1) -- hack, to fail nicely when there's no input data
topoSep (Topo (x:_)) = sectSep x

type MetresI = Int

heightAt :: LatLong -> Topo -> MetresI
heightAt pos (Topo sects) =
    case sectsContaining of
      [] -> error ("heightAt: no section containing position " ++ show pos)
      (s:_) -> sectHeightAt pos s
    where sectsContaining = filter (flip areaContains pos . sectArea) sects

data Sect = Sect { sectArea :: Area
                 , sectSep :: LatLongSize
                 , sectArray :: Arr }

-- TODO check divisions are exact
sectHeightAt :: LatLong -> Sect -> MetresI
sectHeightAt pos sect = sectArray sect ! (y, x)
    where (y, x) = (dLat `quot` latSep, dLong `quot` longSep)
          (latSep, longSep) = latLongSizeToSecs (sectSep sect)
          (dLat, dLong) = (posLat - swLat, posLong - swLong)
          (posLat, posLong) = latLongToSecs pos
          (swLat, swLong) = latLongToSecs (areaSW (sectArea sect))

type Arr = UArray (Int, Int) MetresI

mkSect :: Area -> LatLongSize -> [[Int]] -> Sect
mkSect area sep vals = Sect area sep arr
    where
      arr = array bnds (zip ixsNorthtoSouth (concat vals))
      ixsNorthtoSouth = [(y, x) | y <- [maxy, maxy-1 .. 0], x <- [0..maxx]]
      (latS, longS) = latLongSizeToSecs (areaSize area)
      (latSamps, longSamps) = (latS `quot` latSep, longS `quot` longSep)
      (maxy, maxx) = (latSamps - 1, longSamps - 1)
      bnds = ((0, 0), (maxy, maxx))
      (latSep, longSep) = latLongSizeToSecs sep

type MetresF = Double
type Heights = Array (Int, Int) (MetresF, MetresF, MetresF)

topoHeights :: LatLong -> Area -> Topo -> Except [Area] (Area, Heights)
topoHeights refPoint reqArea topo@(Topo sects) =
    case missingAreas of
      [] -> return (area, topoHeights' refPoint area topo)
      xs -> throwE xs
    where area = expandToGrid (topoSep topo) reqArea
          missingAreas = foldM areaSubtract area sectAreas
          sectAreas = map sectArea sects

-- always called with areas on the grid
topoHeights' :: LatLong -> Area -> Topo -> Heights
topoHeights' refPoint area topo = arrayFromFn bnds pointAt
    where
      bnds = ((0, 0), (sampsLat - 1, sampsLong - 1))
      (sampsLat, sampsLong) = (latSec `quot` latSep, longSec `quot` longSep)
      (latSec, longSec) = latLongSizeToSecs (areaSize area)
      (latSep, longSep) = latLongSizeToSecs (topoSep topo)
      (mPerLatSec, mPerLongSec) = metresPerSecAt (latitude refPoint)
      (mPerLatSamp, mPerLongSamp) = ( mPerLatSec * fromIntegral latSep,
                                      mPerLongSec * fromIntegral longSep )
      pointAt (y, x) = (dblX, dblY, dblZ)
          where
            dblX = fromIntegral x * mPerLongSamp
            dblY = fromIntegral y * mPerLatSamp
            dblZ = fromIntegral (heightAt ll topo)
            ll = fromMaybe (error "lat/long out of range")
                 (areaSW area `addSize` scaleSize (y, x) (topoSep topo))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))
