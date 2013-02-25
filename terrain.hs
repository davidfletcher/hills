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
main = do
  conf <- execParser optInfo
  run conf

run :: Opts -> IO ()
run opts = do
  let centre = optCentre opts
  let size = optSize opts
  let area = fromMaybe (error "bad area") -- TODO
             $ Area.areaFromCentreAndSize centre size
  let listedFiles = optInFiles opts
  let files = case listedFiles of [] -> CGIAR.filesForArea area
                                  fs -> fs
 -- TODO report if no files
  topo <- Parse.readAscs area files
  case Topo.topoHeights area topo of
      Left badAreas -> do
          hPutStrLn stderr "error: no data available for area(s)"
          mapM_ (hPutStrLn stderr . ("  "++) . Area.areaShowUser) badAreas
      Right (usedArea, samps) -> do
          putStrLn ("generating for area: " ++ Area.areaShowUser usedArea)
          writeFile "out.stl" (makeStl opts samps)

makeStl :: Opts -> Topo.Heights -> String
makeStl opts samps = Stl.toString "topo" stl
  where baseAlt = optBaseAlt opts
        stl = Model.model baseAlt samps

data Opts = Opts
    { optCentre :: LatLong
    , optSize :: (Int, Int)
    , optBaseAlt :: Double
    , optInFiles :: [String]
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
    <*> arguments Just ( metavar "INPUT FILES" )

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
