module Parse (readAscs) where

import Area
import LatLong
import Topo

import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe, catMaybes)
import System.Directory (doesFileExist)
import System.IO

secsPerSamp :: Int
secsPerSamp = 3

sep :: LatLongSize
sep = sizeFromSecs (secsPerSamp, secsPerSamp)

readAscs :: Area -> [String] -> IO Topo
readAscs area fileNames =
  mkTopo . catMaybes <$> mapM (parseFile areaToGet) fileNames
      where areaToGet = expandBy sep (expandToGrid sep area)

-- returns Nothing if the file doesn't exist
parseFile :: Area -> String -> IO (Maybe Sect)
parseFile area fileName = do
  exists <- doesFileExist fileName
  if exists
  then do
    contents <- BC.readFile fileName
    return . Just $ parseContents area contents
  else do
    hPutStrLn stderr (fileName ++ " does not exist")
    return Nothing

-- Returns an 0x0 section if the file had nothing from the desired area.
parseContents :: Area -> BC.ByteString -> Sect
parseContents wantedArea s =
  case areaIntersect wholeArea wantedArea of
    Nothing -> mkSect emptyArea sep []
      where
        emptyArea = areaFromSouthwestAndSize (areaSW wantedArea) zeroSize
    Just avail -> mkSect avail sep vals
      where
        region = fileRegion wholeArea avail
        vals = parseLines region rest
  where (wholeArea, rest) = parseHeader (BC.lines s)

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
    where (wantedLat, wantedLong) = sizeToSecs (areaSize wantedArea)
          (wantedLines, wantedSamps) = (wantedLat `quot` secsPerSamp,
                                        wantedLong `quot` secsPerSamp)
          (wholeLat, _) = sizeToSecs (areaSize wholeArea)
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
    areaFromSouthwestAndSize sw (scaleSize (rows, cols) sep)
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
