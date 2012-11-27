module LatLong ( LatLong
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
               )
where

import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Text as T


-- Latitude

newtype Lat = Lat { latSec :: Int } deriving (Eq, Ord)

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

newtype Long = Long { longSec :: Int } deriving (Eq, Ord)

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
               deriving (Eq, Show)

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


-- parsing helper

maybeReadText :: Read a => T.Text -> Maybe a
maybeReadText s = case reads (T.unpack s) of
                    [(x, [])] -> Just x
                    _ -> Nothing
