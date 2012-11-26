module Main where

import LatLong
import qualified Stl
import qualified Model
import qualified Topo

import Control.Monad.Error
import Data.Maybe
import System.Console.GetOpt
import System.Environment

main :: IO ()
main = do
  argv <- getArgs
  case parseArgs argv of
    Left err -> putStr ("error: " ++ err ++ "\n\n"
                        ++ usageInfo usageHeader options)
    Right opts -> run opts

parseArgs :: [String] -> Either String ValidOptions
parseArgs argv = do
  opts <- case getOpt Permute options argv of
            (opts, [], []) -> Right . foldr (.) id opts $ defaultOptions
            (_, _, errs) -> Left (concat errs)
  validateOptions opts

run :: ValidOptions -> IO ()
run opts = do
  let region = Topo.FileRegion { Topo.regFirstLine = 4540, Topo.regNumLines = 100 }
  topo <- Topo.parseFile region "srtm_36_01.asc"
  -- writePGM "out.pgm" topo
  let hs = Topo.topoHeights (4540, 1280) (100, 200) topo
  let stl = Model.model hs
  writeFile "out.stl" (Stl.toString "topo" stl)

type ValidOptions = LatLong

validateOptions :: Options -> Either String ValidOptions
validateOptions opts = do
  c <- optCentre opts
  return c

usageHeader :: String
usageHeader  = "usage: terrain [options] -c LAT,LONG"

data Options = Options {
      optCentre :: Either String LatLong
} deriving Show

setOptCentre :: Either String LatLong -> Options -> Options
setOptCentre v opts = opts { optCentre = v }

defaultOptions :: Options
defaultOptions =
    Options {
      optCentre = Left "no center point supplied"
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['c'] ["center"]
             (ReqArg parseCentre "LAT,LONG")
             "center point"
  ]

parseCentre :: String -> (Options -> Options)
parseCentre = setOptCentre . maybeToEither "bad lat/long" . parseLatLong

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither msg = maybe (Left msg) Right
