module Parse (readAscs) where

import Area
import LatLong
import Topo

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe)

secsPerSamp :: Int
secsPerSamp = 3

readAscs :: Area -> [String] -> IO Topo
readAscs area fileNames =
  fmap mkTopo $ mapM (parseFile area) fileNames

parseFile :: Area -> String -> IO Sect
parseFile area fileName = do
  contents <- BC.readFile fileName
  return $ parseContents area contents

-- Returns an 0x0 section if the file had nothing from the desired area.
parseContents :: Area -> BC.ByteString -> Sect
parseContents wantedArea s =
    case areaIntersect wholeArea wantedArea of
      Nothing -> mkSect emptyArea seps []
          where emptyArea = areaFromSouthwestAndSize (areaSW wantedArea) (0, 0)
      Just avail -> mkSect avail seps vals
          where
            region = fileRegion wholeArea avail
            vals = parseLines region rest
    where (wholeArea, rest) = parseHeader (BC.lines s)
          seps = (secsPerSamp, secsPerSamp)

data FileRegion = FileRegion { regFirstLine :: Int
                             , regNumLines :: Int
                             , regFirstSamp :: Int
                             , regNumSamps :: Int }
                  deriving Show

-- Assumes wantedArea is wholly contained in wholeArea.
fileRegion :: Area -> Area -> FileRegion
fileRegion wholeArea wantedArea =
    FileRegion
    { regFirstLine = (wholeN - wantedN) `quot` secsPerSamp
    , regNumLines = wantedLines
    , regFirstSamp = (wantedW - wholeW) `quot` secsPerSamp
    , regNumSamps = wantedSamps }
    where (wantedLat, wantedLong) = areaSize wantedArea
          (wantedLines, wantedSamps) = (wantedLat `quot` secsPerSamp,
                                        wantedLong `quot` secsPerSamp)
          (wholeLat, _) = areaSize wholeArea
          wholeLines = wholeLat `quot` secsPerSamp
          (wholeS, wholeW) = latLongToSecs (areaSW wholeArea)
          (wantedS, wantedW) = latLongToSecs (areaSW wantedArea)
          wantedN = wantedS + wantedLines * secsPerSamp
          wholeN = wholeS + wholeLines * secsPerSamp

parseHeader :: [BC.ByteString] -> (Area, [BC.ByteString])
parseHeader ls =
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
      (headerLines, rest) = splitAt 6 ls

headerArea :: Double -> Double -> Int -> Int -> Area
headerArea latDeg longDeg rows cols =
    areaFromSouthwestAndSize sw (rows * secsPerSamp, cols * secsPerSamp)
    where
      sw = fromMaybe (error "bad header area") -- TODO
           (latLongFromDoubleDegs secsPerSamp (latDeg, longDeg))

parseLines :: FileRegion -> [BC.ByteString] -> [[Int]]
parseLines region =
    map (parseLine region) . take (regNumLines region) . drop (regFirstLine region)

parseLine :: FileRegion -> BC.ByteString -> [Int]
parseLine region = map (read . BC.unpack)
                   . take (regNumSamps region)
                   . drop (regFirstSamp region)
                   . BC.words
