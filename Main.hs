module Main where

import qualified Area
import qualified CGIAR
import LatLong
import qualified Parse
import qualified Stl
import qualified Model
import qualified Topo

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Options.Applicative
import System.IO
import Numeric (showFFloat)

main :: IO ()
main = execParser optInfo >>= run

run :: Opts -> IO ()
run opts = either (hPutStrLn stderr) return =<< runExceptT (run' opts)

type Err a = Except String a
type ErrT a = ExceptT String a

hoistExcept :: Except String a -> ExceptT String IO a
hoistExcept = ExceptT . return . runExcept

run' :: Opts -> ErrT IO ()
run' opts = do
  (refPoint, area, fs) <- hoistExcept (getAreaAndFiles opts)
  topo <- liftIO (Parse.readAscs area fs)
  (usedArea, samps) <- hoistExcept (getTopo refPoint area topo)
  liftIO (putStr (reportArea refPoint usedArea))
  liftIO (L.writeFile (optOutFile opts) (makeStl opts samps))

reportArea :: LatLong -> Area.Area -> String
reportArea refPt a =
  unlines [ "generating for ",
            "  " ++ Area.areaShowUser a,
            "  " ++ show latSec ++ " arcsec N/S x " ++ show longSec ++ " arcsec E/W",
            "  " ++ fmtKm latKm ++ "km N/S x " ++ fmtKm longKm ++ "km E/W" ]
  where
    (latSec, longSec) = sizeToSecs (Area.areaSize a)
    (latM, longM) = sizeInMetres refPt (Area.areaSize a)
    (latKm, longKm) = (latM / 1000, longM / 1000)
    fmtKm n = showFFloat (Just 3) n ""

getTopo :: LatLong -> Area.Area -> Topo.Topo -> Err (Area.Area, Topo.Heights)
getTopo refPoint area topo =
  withExcept showBad (Topo.topoHeights refPoint area topo)
  where
    showBad badAreas = unlines (msg : map showOne badAreas)
    showOne = ("  "++) . Area.areaShowUser
    msg = "error: no data available for area(s)"

getAreaAndFiles :: Opts -> Err (LatLong, Area.Area, [FilePath])
getAreaAndFiles opts = do
  area <- getArea (optCentre opts) (optSize opts) (optOffset opts)
  files <- getFiles area
  return (optCentre opts, area, map addDir files)
  where
    addDir f = case optInDir opts of Nothing -> f
                                     Just dir -> dir ++ "/" ++ f

getFiles :: Area.Area -> Err [FilePath]
getFiles = maybeToExcept "area not covered by CGIAR data" . CGIAR.filesForArea

getArea :: LatLong -> LatLongSize -> Maybe LatLongD -> Err Area.Area
getArea centre size Nothing =
    maybeToExcept "bad area" (Area.areaFromCentreAndSize centre size)
getArea centre size (Just offset) =
  maybeToExcept "bad area" (
    Area.areaFromSouthwestAndSize <$> addD centre offset  <*> pure size)

maybeToExcept :: a -> Maybe b -> Except a b
maybeToExcept err = maybe (throwE err) return

makeStl :: Opts -> Topo.Heights -> L.ByteString
makeStl opts samps = Stl.toBinary stl
  where
    stl = Model.model (optScale opts) (optBaseAlt opts) samps

data Opts = Opts
    { optCentre :: LatLong
    , optSize :: LatLongSize
    , optOffset :: Maybe LatLongD
    , optBaseAlt :: Double
    , optScale :: Double
    , optInDir :: Maybe FilePath
    , optOutFile :: FilePath
    } deriving Show

optInfo :: ParserInfo Opts
optInfo = info (helper <*> optParser) fullDesc

optParser :: Parser Opts
optParser =
    Opts
    <$> option (eitherReader parseLatLongOpt)
               ( short 'p'
                 <> long "position"
                 <> metavar "LAT,LONG"
                 <> help "position (by default the centre)" )
    <*> option (eitherReader parseSizeOpt)
               ( short 'd'
                 <> long "dimensions"
                 <> value (sizeFromSecs (300, 600))
                 <> metavar "ARCSECxARCSEC"
                 <> help "size in arcseconds" )
    <*> option (eitherReader parseOffsetOpt)
               ( long "offset"
                 <> value Nothing
                 <> metavar "ARCSEC,ARCSEC"
                 <> help "offset from centre in arcseconds" )
    <*> option auto
               ( short 'b'
                 <> long "base-altitude"
                 <> value ((-100) :: Double)
                 <> metavar "METRES"
                 <> help "base altitude" )
    <*> option auto
               ( short 's'
                 <> long "scale"
                 <> value 100
                 <> metavar "FACTOR"
                 <> help "factor to scale down by" )
    <*> option (eitherReader (Right . Just))
               ( short 'i'
                 <> long "input-dir"
                 <> value Nothing
                 <> metavar "DIR"
                 <> help "directory with input files" )
    <*> argument Just ( metavar "OUTFILE" )

parseLatLongOpt :: String -> Either String LatLong
parseLatLongOpt s = case parseLatLong s of
                      Nothing -> Left ("bad lat/long '" ++ s ++ "'")
                      Just x -> Right x

parseOffsetOpt :: String -> Either String (Maybe LatLongD)
parseOffsetOpt s = case parseDelta s of
                     Nothing -> Left ("bad offset '" ++ s ++ "'")
                     Just x -> Right (Just x)

parseSizeOpt :: String -> Either String LatLongSize
parseSizeOpt s = case parseSize s of
                   Nothing -> Left ("bad size '" ++ s ++ "'")
                   Just x -> Right x
