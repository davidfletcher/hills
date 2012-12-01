module Main where

import LatLong
import qualified Parse
import qualified Stl
import qualified Model
import qualified Topo

import Data.Maybe
import Data.Monoid
import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  argv <- getArgs
  case parseArgs argv of
    Left err -> putStr ("error: " ++ err ++ "\n\n"
                        ++ usageInfo usageHeader options)
    Right opts -> run opts

parseArgs :: [String] -> Either String Opts
parseArgs argv = do
  opts <- case getOpt Permute options argv of
            (es, [], []) -> Right $ appEndo (mconcat es) defaultOpts
            (_, _, errs) -> Left $ concat errs
  validateOpts opts

run :: Opts -> IO ()
run opts = do
  let file = head $ optInFiles opts -- TODO
  let centre = optCentre opts
  let size = optSize opts
  let area = fromJust $ Topo.areaFromCentreAndSize centre size
  topo <- Parse.readAsc area file
  let stl = makeStl opts area topo
  writeFile "out.stl" stl

makeStl :: Opts -> Topo.Area -> Topo.Topo -> String
makeStl opts area topo = Stl.toString "topo" stl
  where hs = Topo.topoHeights area topo
        baseAlt = optBaseAlt opts
        stl = Model.model baseAlt hs

data Opts = Opts { optCentre :: LatLong
                 , optSize :: (Int, Int)
                 , optInFiles :: [String]
                 , optBaseAlt :: Double }

validateOpts :: ParsedOpts -> Either String Opts
validateOpts opts = do
  c <- poptCentre opts
  sz <- poptSize opts
  baseAlt <- poptBaseAlt opts
  let files = poptInFiles opts
  case files of [] -> Left "no input files listed"
                _ -> Right ()
  return $ Opts { optCentre = c
                , optSize = sz
                , optInFiles = files
                , optBaseAlt = baseAlt }

usageHeader :: String
usageHeader  = "usage: terrain [options] -c LAT,LONG"

data ParsedOpts = ParsedOpts
    { poptCentre :: Either String LatLong
    , poptSize :: Either String (Int, Int)
    , poptInFiles :: [String]
    , poptBaseAlt :: Either String Double
    } deriving Show

setCentre :: Either String LatLong -> ParsedOpts -> ParsedOpts
setCentre v opts = opts { poptCentre = v }

setSize :: Either String (Int, Int) -> ParsedOpts -> ParsedOpts
setSize v opts = opts { poptSize = v }

setBaseAlt :: Either String Double -> ParsedOpts -> ParsedOpts
setBaseAlt v opts = opts { poptBaseAlt = v }

addInFile :: String -> ParsedOpts -> ParsedOpts
addInFile v opts = opts { poptInFiles = v : poptInFiles opts }

defaultOpts :: ParsedOpts
defaultOpts =
    ParsedOpts
    { poptCentre = Left "no center point supplied"
    , poptSize = Right (100, 200)
    , poptInFiles = []
    , poptBaseAlt = Right (-100)
    }

options :: [OptDescr (Endo ParsedOpts)]
options = [
  Option ['c'] ["center"]
             (ReqArg (Endo . setCentre . parseLatLongOpt) "LAT,LONG")
             "center point",
  Option ['s'] ["size"]
             (ReqArg (Endo . setSize . parseSizeOpt) "LATSAMPSxLONGSAMPS")
             "size in samples",
  Option ['b'] ["base-altitude"]
             (ReqArg (Endo . setBaseAlt . parseDouble) "METRES")
             "base altitude",
  Option ['i'] ["in-file"]
             (ReqArg (Endo . addInFile) "FILENAME")
             "input data file"
  ]

parseLatLongOpt :: String -> Either String LatLong
parseLatLongOpt s = case parseLatLong s of
                      Nothing -> Left ("bad lat/long '" ++ s ++ "'")
                      Just x -> Right x

parseSizeOpt :: String -> Either String (Int, Int)
parseSizeOpt s = case (reads latPart, reads longPart) of
                   ( [(lat, [])], [(long, [])] ) -> Right (lat, long)
                   _ -> Left $ "bad size '" ++ s ++ "'"
    where
      (latPart, rest) = break (== 'x') s
      longPart = case rest of [] -> []
                              (_:xs) -> xs

parseDouble :: String -> Either String Double
parseDouble s = case reads s of
                  [(v, [])] -> Right v
                  _ -> Left $ "bad number '" ++ s ++ "'"
