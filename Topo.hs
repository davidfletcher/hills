module Topo ( Topo
            , areaFromCentreAndSize
            , mkTopo
            , topoHeights
            , parseFile
            , Heights )
where

import LatLong

import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.Word
import Debug.Trace

newtype Topo = Topo { topoSects :: [Sect] }

mkTopo :: Topo
mkTopo = Topo []

topoArray :: Topo -> Arr
topoArray = sectArray . head . topoSects -- TODO

data Sect = Sect { sectArea :: Area, sectArray :: Arr }

type Metres = Int

-- TODO check divisions are exact
heightAt :: LatLong -> Sect -> Metres
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

type Arr = UArray (Int, Int) Metres

data FileRegion = FileRegion { regFirstLine :: Int
                             , regNumLines :: Int
                             , regFirstSamp :: Int
                             , regNumSamps :: Int }

regLastLine :: FileRegion -> Int
regLastLine reg = regFirstLine reg + regNumLines reg - 1

-- TODO
testRegion = FileRegion { regFirstLine = 4540
                        , regNumLines = 100
                        , regFirstSamp = 1280
                        , regNumSamps = 200 }

parseFile :: Area -> String -> Topo -> IO Topo
parseFile area fileName (Topo sects) = do
  contents <- BC.readFile fileName
  let sect = parseContents area contents
  return $ Topo (sect:sects)

-- TODO check desired area is in header area
parseContents :: Area -> BC.ByteString -> Sect
parseContents wantedArea s =
    let (wholeArea, rest) = parseHeader (BC.lines s)
        vals = parseLines testRegion rest
    in mkSect wantedArea vals

mkSect :: Area -> [[Int]] -> Sect
mkSect area vals = Sect area arr
    where
      arr = array bnds (zip ixsNorthtoSouth (concat vals))
      ixsNorthtoSouth = [(y, x) | y <- [maxy, maxy-1 .. 0], x <- [0..maxx]]
      (szy, szx) = areaSize area
      (maxy, maxx) = (szy - 1, szx - 1)
      bnds = ((0, 0), (maxy, maxx))


parseHeader :: [BC.ByteString] -> (Area, [BC.ByteString])
parseHeader lines =
    case map (words . BC.unpack) headerLines of
      [ ["ncols", ncols],
        ["nrows", nrows],
        ["xllcorner", xllcorner],
        ["yllcorner", yllcorner],
        ["cellsize", _],
        ["NODATA_value", _] ]
          -> ( headerArea
                 (read yllcorner) -- TODO check these reads
                 (read xllcorner)
                 (read nrows)
                 (read ncols),
               rest)
      _ -> error "failed to parse header" -- TODO
    where
      (headerLines, rest) = splitAt 6 lines

headerArea :: Int -> Int -> Int -> Int -> Area
headerArea latDeg longDeg rows cols =
    Area { areaSW = sw, areaSize = (rows, cols) }
    where
      sw = fromJust (latLongFromDegMinSec latDMS longDMS)
      latDMS = (if latDeg >= 0 then North else South, abs latDeg, 0, 0)
      longDMS = (if longDeg >= 0 then East else West, abs longDeg, 0, 0)

parseLines :: FileRegion -> [BC.ByteString] -> [[Int]]
parseLines region =
    map (parseLine region) . take (regNumLines region) . drop (regFirstLine region)

parseLine :: FileRegion -> BC.ByteString -> [Int]
parseLine region = map (read . BC.unpack)
                   . take (regNumSamps region)
                   . drop (regFirstSamp region)
                   . BC.words

{- TODO

toPixels :: Arr -> [Word8]
toPixels a = concat (mapArrToLists heightToPix a)

mapArrToLists :: (Int -> a) -> Arr -> [[a]]
mapArrToLists f a = map (map f) lists
    where lists = map (map (a !)) (coords a)

coords :: Arr -> [[(Int, Int)]]
coords a = [[(y, x) | x <- [minx..maxx]] | y <- [miny..maxy]]
    where ((miny, minx), (maxy, maxx)) = bounds a

heightToPix :: Int -> Word8
heightToPix x | x < 0 = 0
              | x >= 0 && x <= 8*128 = 127 + fromIntegral (x `div` 8)
              | otherwise = 255

toPGM :: Arr -> B.ByteString
toPGM arr = B.concat [BC.pack header, B.pack (toPixels arr)]
    where header = concat [ "P5\n",
                            show width, " ", show height, "\n",
                            "255\n" ]
          width = (maxx - minx) + 1
          height = (maxy - miny) + 1
          ((miny, minx), (maxy, maxx)) = bounds arr

writePGM :: String -> Sect -> IO ()
writePGM fileName (Sect arr) = B.writeFile fileName (toPGM arr)
-}

type Heights = Array (Int, Int) (Double, Double, Double)

-- metres
-- TODO for other lat/longs
degreeLatAt56N = 111360
degreeLongAt56N = 60772
threeArcsecLatAt56N = 3 * degreeLatAt56N / 3600
threeArcsecLongAt56N = 3 * degreeLongAt56N / 3600

topoHeights :: Area -> Topo -> Heights
topoHeights area (Topo sects) = arrayFromFn bnds pointAt
    where
      sect = head sects -- TODO
      bnds = ((0, 0), (szLat - 1, szLong - 1))
      (szLat, szLong) = areaSize area
      (south, west) = latLongToSecs (areaSW area)
      pointAt (y, x) = (dblX, dblY, dblZ)
          where
            dblX = fromIntegral x * threeArcsecLongAt56N
            dblY = fromIntegral y * (-threeArcsecLatAt56N)
            dblZ = fromIntegral (heightAt ll sect)
            ll = fromMaybe (error "lat/long out of range")
                 (latLongFromSecs (south + y * secsPerSamp, west + x * secsPerSamp))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))
