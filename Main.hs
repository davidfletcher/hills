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
  (area, fs) <- hoistExcept (getAreaAndFiles opts)
  topo <- liftIO (Parse.readAscs area fs)
  (usedArea, samps) <- hoistExcept (getTopo area topo)
  liftIO (putStrLn ("generating for area: " ++ Area.areaShowUser usedArea))
  liftIO (L.writeFile (optOutFile opts) (makeStl opts samps))

getTopo :: Area.Area -> Topo.Topo -> Err (Area.Area, Topo.Heights)
getTopo area topo = withExcept showBad (Topo.topoHeights area topo)
    where
      showBad badAreas = unlines (msg : map showOne badAreas)
      showOne = ("  "++) . Area.areaShowUser
      msg = "error: no data available for area(s)"

getAreaAndFiles :: Opts -> Err (Area.Area, [FilePath])
getAreaAndFiles opts = do
  area <- getArea (optCentre opts) (optSize opts)
  files <- getFiles area
  return (area, files)

getFiles :: Area.Area -> Err [FilePath]
getFiles = maybeToExcept "area not covered by CGIAR data" . CGIAR.filesForArea

getArea :: LatLong -> (Int, Int) -> Err Area.Area
getArea centre size =
    maybeToExcept "bad area" $ Area.areaFromCentreAndSize centre size

maybeToExcept :: a -> Maybe b -> Except a b
maybeToExcept err = maybe (throwE err) return

makeStl :: Opts -> Topo.Heights -> L.ByteString
makeStl opts samps = Stl.toByteString "topo" stl
  where baseAlt = optBaseAlt opts
        stl = Model.model baseAlt samps

data Opts = Opts
    { optCentre :: LatLong
    , optSize :: (Int, Int)
    , optBaseAlt :: Double
    , optInDir :: Maybe FilePath
    , optOutFile :: FilePath
    } deriving Show

optInfo :: ParserInfo Opts
optInfo = info (helper <*> optParser) fullDesc

optParser :: Parser Opts
optParser =
    Opts
    <$> option (eitherReader parseLatLongOpt)
               ( short 'c'
                 <> long "center"
                 <> metavar "LAT,LONG"
                 <> help "center point" )
    <*> option (eitherReader parseSizeOpt)
               ( short 's'
                 <> long "size"
                 <> value (300, 600)
                 <> metavar "LATSAMPSxLONGSAMPS"
                 <> help "size in samples" )
    <*> option auto
               ( short 'b'
                 <> long "base-altitude"
                 <> value ((-100) :: Double)
                 <> metavar "METERS"
                 <> help "base altitude" )
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

parseSizeOpt :: String -> Either String (Int, Int)
parseSizeOpt s = case (reads latPart, reads longPart) of
                   ( [(lat, [])], [(lon, [])] ) -> Right (lat, lon)
                   _ -> Left ("bad size '" ++ s ++ "'")
    where
      (latPart, rest) = break (== 'x') s
      longPart = case rest of [] -> []
                              (_:xs) -> xs
