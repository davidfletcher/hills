module Main where

import qualified Area
import qualified CGIAR
import LatLong
import qualified Parse
import qualified Stl
import qualified Model
import qualified Topo

import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.IO

main :: IO ()
main = execParser optInfo >>= run

run :: Opts -> IO ()
run opts =
  case getAreaAndFiles opts of
    Left err -> hPutStrLn stderr err
    Right (area, fs) -> do
        topo <- Parse.readAscs area fs

        case Topo.topoHeights area topo of
          Left badAreas -> do
              -- should only happen if we didn't have the required input files
              hPutStrLn stderr "error: no data available for area(s)"
              mapM_ (hPutStrLn stderr . ("  "++) . Area.areaShowUser) badAreas

          Right (usedArea, samps) -> do
              putStrLn ("generating for area: " ++ Area.areaShowUser usedArea)
              writeFile (optOutFile opts) (makeStl opts samps)

getAreaAndFiles :: Opts -> Either String (Area.Area, [FilePath])
getAreaAndFiles opts = do
  area <- getArea (optCentre opts) (optSize opts)
  files <- getFiles area
  return (area, files)

getFiles :: Area.Area -> Either String [FilePath]
getFiles = maybeToErr "area not covered by CGIAR data" . CGIAR.filesForArea

getArea :: LatLong -> (Int, Int) -> Either String Area.Area
getArea centre size =
    maybeToErr "bad area" $ Area.areaFromCentreAndSize centre size

maybeToErr :: a -> Maybe b -> Either a b
maybeToErr err = maybe (Left err) Right

makeStl :: Opts -> Topo.Heights -> String
makeStl opts samps = Stl.toString "topo" stl
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
    <$> nullOption ( short 'c'
                     <> long "center"
                     <> reader parseLatLongOpt
                     <> metavar "LAT,LONG"
                     <> help "center point" )
    <*> option ( short 's'
                 <> long "size"
                 <> reader parseSizeOpt
                 <> value (300, 600)
                 <> metavar "LATSAMPSxLONGSAMPS"
                 <> help "size in samples" )
    <*> option ( short 'b'
                 <> long "base-altitude"
                 <> value ((-100) :: Double)
                 <> metavar "METERS"
                 <> help "base altitude" )
    <*> option ( short 'i'
                 <> long "input-dir"
                 <> reader (Right . Just)
                 <> value Nothing
                 <> metavar "DIR"
                 <> help "directory with input files" )
    <*> argument Just ( metavar "OUTFILE" )

parseLatLongOpt :: String -> Either ParseError LatLong
parseLatLongOpt s = case parseLatLong s of
                      Nothing -> Left $ ErrorMsg ("bad lat/long '" ++ s ++ "'")
                      Just x -> Right x

parseSizeOpt :: String -> Either ParseError (Int, Int)
parseSizeOpt s = case (reads latPart, reads longPart) of
                   ( [(lat, [])], [(long, [])] ) -> Right (lat, long)
                   _ -> Left . ErrorMsg $ "bad size '" ++ s ++ "'"
    where
      (latPart, rest) = break (== 'x') s
      longPart = case rest of [] -> []
                              (_:xs) -> xs
