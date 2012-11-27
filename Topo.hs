module Topo ( Topo
            , mkTopo
            , topoHeights
            , parseFile
            , writePGM
            , Heights )
where

import LatLong

import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char
import Data.Maybe (fromJust)
import Data.Word

newtype Topo = Topo { topoSects :: [Sect] }

mkTopo :: Topo
mkTopo = Topo []

topoArray :: Topo -> Arr
topoArray = sectArray . head . topoSects -- TODO

data Sect = Sect { sectPos :: LatLong
                 , sectArray :: Arr }

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

type Arr = UArray (Int, Int) Int

data FileRegion = FileRegion { regFirstLine :: Int
                             , regNumLines :: Int }

regLastLine :: FileRegion -> Int
regLastLine reg = regFirstLine reg + regNumLines reg - 1

-- TODO
testRegion = FileRegion { regFirstLine = 4540, regNumLines = 100 }

parseFile :: LatLong -> String -> Topo -> IO Topo
parseFile centre fileName (Topo sects) = do
  contents <- BC.readFile fileName
  let area = fromJust $ areaFromCentreAndSize centre (100, 200) -- TODO
  let sect = parseContents area contents
  return $ Topo (sect:sects)

-- TODO check desired area is in header area
parseContents :: Area -> BC.ByteString -> Sect
parseContents wantedArea s =
    let (wholeArea, rest) = parseHeader (BC.lines s)
        vals = parseLines testRegion rest
        arrBounds = ((regFirstLine testRegion, 0), -- TODO
                     (regLastLine testRegion, 5999))
    in Sect (areaSW wantedArea) (listArray arrBounds (concat vals))

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
    map parseLine . take (regNumLines region) . drop (regFirstLine region)

parseLine :: BC.ByteString -> [Int]
parseLine = map (read . BC.unpack) . BC.words

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
writePGM fileName (Sect _ arr) = B.writeFile fileName (toPGM arr)

type Heights = Array (Int, Int) (Double, Double, Double)

-- metres
-- TODO for other lat/longs
degreeLatAt56N = 111360
degreeLongAt56N = 60772
threeArcsecLatAt56N = 3 * degreeLatAt56N / 3600
threeArcsecLongAt56N = 3 * degreeLongAt56N / 3600

topoHeights :: (Int, Int) -> (Int, Int) -> Topo -> Heights
topoHeights (minLine, minSamp) (lineCount, sampCount) (Topo sects) =
    arrayFromFn bnds pointAt
    where
      Sect _ arr = head sects -- TODO
      bnds = ( (minLine, minSamp), (maxLine, maxSamp) )
      (maxLine, maxSamp) = (minLine + lineCount - 1, minSamp + sampCount - 1)
      pointAt (y, x) = (fromIntegral (x-minSamp) * threeArcsecLongAt56N,
                        fromIntegral (y-minLine) * (-threeArcsecLatAt56N),
                        heightFromVal (arr ! (y, x)))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))

heightFromVal :: Int -> Double
heightFromVal (-9999) = 0
heightFromVal x = fromIntegral x
