module LatLong ( LatLong
               , latitude
               , longitude
               , Lat
               , Long
               , NorthSouth(..)
               , EastWest(..)
               , parseLatLong
               , latToDegMinSec
               , longToDegMinSec
               , latLongFromSecs
               , latLongToSecs
               , latLongFromDegMinSec
               , latLongFromDoubleDegs
               , metresPerSecAt
               )
where

import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Text as T


-- Latitude

newtype Lat = Lat Int deriving (Eq, Ord)

instance Show Lat where
    show lat =
        "mkLat (" ++
        show ns ++ "," ++
        show latd ++ "," ++
        show latm ++ "," ++
        show lats ++ ")"
            where (ns, latd, latm, lats) = latToDegMinSec lat

data NorthSouth = North | South deriving (Eq, Show)

mkLat :: (NorthSouth, Int, Int, Int) -> Lat
mkLat (ns, d, m, s) =
    fromMaybe (error "mkLat: latitude out of range") (latFromSecs v)
    where
      v = withSign (d * 3600 + m * 60 + s)
      withSign = case ns of North -> id
                            South -> negate

latFromSecs :: Int -> Maybe Lat
latFromSecs s | s > 90*3600 || s < (-90*3600) = Nothing
              | otherwise = Just (Lat s)

latToDegMinSec :: Lat -> (NorthSouth, Int, Int, Int)
latToDegMinSec (Lat s) = ( if s < 0 then South else North,
                           abs s `div` 3600,
                           (abs s `div` 60) `rem` 60,
                           abs s `rem` 60 )

-- Longitude

newtype Long = Long Int deriving (Eq, Ord)

instance Show Long where
    show long =
        "mkLong (" ++
        show ew ++ "," ++
        show longd ++ "," ++
        show longm ++ "," ++
        show longs ++ ")"
            where (ew, longd, longm, longs) = longToDegMinSec long

data EastWest = East | West deriving (Eq, Show)

mkLong :: (EastWest, Int, Int, Int) -> Long
mkLong (ew, d, m, s) = longFromSecs v
    where
      v = withSign (d * 3600 + m * 60 + s)
      withSign = case ew of East -> id
                            West -> negate

longFromSecs :: Int -> Long
longFromSecs s = Long normalised
    where
      normalised = (s + minVal) `mod` rangeSize - minVal
      minVal = 180*60*60 - 1 -- arbitrarily have 180-0-0 be east, not west
      rangeSize = 360*60*60

longToDegMinSec :: Long -> (EastWest, Int, Int, Int)
longToDegMinSec (Long s) = ( if s < 0 then West else East,
                             abs s `div` 3600,
                             (abs s `div` 60) `rem` 60,
                             abs s `rem` 60 )


-- Latitude-and-longitude

data LatLong = LatLong { latitude :: Lat, longitude :: Long }
               deriving (Eq, Ord, Show)

latLongFromSecs :: (Int, Int) -> Maybe LatLong
latLongFromSecs (lats, longs) = do
  lat <- latFromSecs lats
  let long = longFromSecs longs
  return $ LatLong lat long

-- TODO handle mkLat failure better
latLongFromDegMinSec :: (NorthSouth, Int, Int, Int)
                     -> (EastWest, Int, Int, Int)
                     -> Maybe LatLong
latLongFromDegMinSec lat long = Just $ LatLong (mkLat lat) (mkLong long)

latLongFromDoubleDegs :: Int -> (Double, Double) -> Maybe LatLong
latLongFromDoubleDegs secMult (latDeg, longDeg) =
    latLongFromSecs (multLatSec, multLongSec)
        where latSec = round (latDeg * 3600)
              longSec = round (longDeg * 3600)
              multLatSec = (latSec `quot` secMult) * secMult
              multLongSec = (longSec `quot` secMult) * secMult

latLongToSecs :: LatLong -> (Int, Int)
latLongToSecs (LatLong (Lat lats) (Long longs)) = (lats, longs)

parseLatLong :: String -> Maybe LatLong
parseLatLong s = do
  let txt = T.pack s
  let parts = T.split (== ',') . T.filter (not . isSpace) $ txt
  guard (length parts == 2)
  let [latPart, longPart] = parts
  latDeg <- maybeReadText latPart
  longDeg <- maybeReadText longPart
  latLongFromSecs (degToArcsec latDeg, degToArcsec longDeg)

degToArcsec :: Double -> Int
degToArcsec d = round (d * 3600)

secToRad :: Int -> Double
secToRad s = deg * ( pi / 180 )
    where deg = fromIntegral s / 3600

-- Formulae are from
-- http://en.wikipedia.org/wiki/Latitude#The_length_of_a_degree_of_latitude
--
metresPerSecAt :: Lat -> (Double, Double)
metresPerSecAt (Lat latSec) = (mPerLatSec, mPerLongSec)
    where theta = secToRad latSec
          oneSecInRad = pi / (180*3600)
          mPerLatSec = (oneSecInRad * a * ( 1 - eccSquared ))
                       / (1 - eccSquared * sinThetaSquared)**(3/2)
          mPerLongSec = (oneSecInRad * a * cos theta)
                        / sqrt(1 - eccSquared * sinThetaSquared)
          eccSquared = (a*a - b*b) / (a*a)
          sinThetaSquared = sin theta ^ (2 :: Int)
          (a, b) = (wgs84maj, wgs84min)

wgs84maj, wgs84min :: Double
wgs84maj = 6378137.0
wgs84min = 6356752.3142

-- parsing helper

maybeReadText :: Read a => T.Text -> Maybe a
maybeReadText s = case reads (T.unpack s) of
                    [(x, [])] -> Just x
                    _ -> Nothing
