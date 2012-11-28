module Main where

import LatLong
import qualified Stl
import qualified Model
import qualified Topo

import Control.Monad.Error
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
  let centre = optCentre opts
  let empty = Topo.mkTopo
  let area = fromJust $ Topo.areaFromCentreAndSize centre (100, 200)
  topo <- Topo.parseFile area "srtm_36_01.asc" empty
  -- writePGM "out.pgm" topo
  let hs = Topo.topoHeights area topo
  let stl = Model.model hs
  writeFile "out.stl" (Stl.toString "topo" stl)

data Opts = Opts { optCentre :: LatLong }

validateOpts :: ParsedOpts -> Either String Opts
validateOpts opts = do
  c <- poptCentre opts
  return $ Opts { optCentre = c }

usageHeader :: String
usageHeader  = "usage: terrain [options] -c LAT,LONG"

data ParsedOpts = ParsedOpts {
      poptCentre :: Either String LatLong
} deriving Show

setCentre :: Either String LatLong -> ParsedOpts -> ParsedOpts
setCentre v opts = opts { poptCentre = v }

defaultOpts :: ParsedOpts
defaultOpts =
    ParsedOpts {
      poptCentre = Left "no center point supplied"
    }

options :: [OptDescr (Endo ParsedOpts)]
options = [
  Option ['c'] ["center"]
             (ReqArg (Endo . setCentre . parseLatLongOpt) "LAT,LONG")
             "center point"
  ]

parseLatLongOpt :: String -> Either String LatLong
parseLatLongOpt s = case parseLatLong s of
                      Nothing -> Left ("bad lat/long '" ++ s ++ "'")
                      Just x -> Right x
