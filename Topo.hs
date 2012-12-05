module Topo ( Topo
            , mkTopo
            , Sect
            , mkSect
            , topoHeights
            , Heights
            )
where

import Area
import LatLong

import Data.Array.Unboxed
import Data.Maybe (fromMaybe)

newtype Topo = Topo [Sect]

mkTopo :: [Sect] -> Topo
mkTopo = Topo

topoSep :: Topo -> ArcsecSize
topoSep (Topo []) = error "topoSep: no sections"
topoSep (Topo (x:_)) = sectSep x

type MetresI = Int

heightAt :: LatLong -> Topo -> MetresI
heightAt pos (Topo sects) =
    case sectsContaining of
      [] -> error "heightAt: no section containing position"
      (s:_) -> sectHeightAt pos s
    where sectsContaining = filter (flip areaContains pos . sectArea) sects

data Sect = Sect { sectArea :: Area
                 , sectSep :: ArcsecSize
                 , sectArray :: Arr }

-- TODO check divisions are exact
sectHeightAt :: LatLong -> Sect -> MetresI
sectHeightAt pos sect = sectArray sect ! (y, x)
    where (y, x) = (dLat `quot` latSep, dLong `quot` longSep)
          (latSep, longSep) = sectSep sect
          (dLat, dLong) = (posLat - swLat, posLong - swLong)
          (posLat, posLong) = latLongToSecs pos
          (swLat, swLong) = latLongToSecs (areaSW (sectArea sect))

type Arr = UArray (Int, Int) MetresI

mkSect :: Area -> ArcsecSize -> [[Int]] -> Sect
mkSect area (latSep, longSep) vals = Sect area (latSep, longSep) arr
    where
      arr = array bnds (zip ixsNorthtoSouth (concat vals))
      ixsNorthtoSouth = [(y, x) | y <- [maxy, maxy-1 .. 0], x <- [0..maxx]]
      (latS, longS) = areaSize area
      (latSamps, longSamps) = (latS `quot` latSep, longS `quot` longSep)
      (maxy, maxx) = (latSamps - 1, longSamps - 1)
      bnds = ((0, 0), (maxy, maxx))

type MetresF = Double
type Heights = Array (Int, Int) (MetresF, MetresF, MetresF)

topoHeights :: Area -> Topo -> Heights
topoHeights area topo = arrayFromFn bnds pointAt
    where
      bnds = ((0, 0), (sampsLat - 1, sampsLong - 1))
      (sampsLat, sampsLong) = (latSec `quot` latSep, longSec `quot` longSep)
      (latSec, longSec) = areaSize area
      (latSep, longSep) = topoSep topo
      (south, west) = latLongToSecs (areaSW area)
      refPoint = areaSW area -- TODO use centre
      (mPerLatSec, mPerLongSec) = metresPerSecAt (latitude refPoint)
      (mPerLatSamp, mPerLongSamp) = ( mPerLatSec * fromIntegral latSep,
                                      mPerLongSec * fromIntegral longSep )
      pointAt (y, x) = (dblX, dblY, dblZ)
          where
            dblX = fromIntegral x * mPerLongSamp
            dblY = fromIntegral y * mPerLatSamp
            dblZ = fromIntegral (heightAt ll topo)
            ll = fromMaybe (error "lat/long out of range")
                 (latLongFromSecs (south + y * latSep, west + x * longSep))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))
