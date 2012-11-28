module Parse (readAsc) where

import LatLong
import Topo

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe)

readAsc :: Area -> String -> IO Topo
readAsc area fileName = do
    sect <- parseFile area fileName
    return $ mkTopo [sect]

parseFile :: Area -> String -> IO Sect
parseFile area fileName = do
  contents <- BC.readFile fileName
  return $ parseContents area contents

-- TODO check desired area is in header area
parseContents :: Area -> BC.ByteString -> Sect
parseContents wantedArea s =
    let (wholeArea, rest) = parseHeader (BC.lines s)
        vals = parseLines testRegion rest
    in mkSect wantedArea vals

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
    areaFromSouthwestAndSize sw (rows, cols)
    where
      sw = fromMaybe (error "bad header area") -- TODO
           (latLongFromDegMinSec latDMS longDMS)
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
