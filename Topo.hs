module Topo ( Topo
            , topoHeights
            , FileRegion(..)
            , parseFile
            , writePGM
            , Heights )
where

import Data.Array.Unboxed
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Char
import Data.Word
import Text.Parsec
import Text.Parsec.ByteString.Lazy

newtype Topo = Topo { topoArray :: Arr }

type Arr = UArray (Int, Int) Int

data FileRegion = FileRegion { regFirstLine :: Int
                             , regNumLines :: Int }

regLastLine :: FileRegion -> Int
regLastLine reg = regFirstLine reg + regNumLines reg - 1

parseFile :: FileRegion -> String -> IO Topo
parseFile region fileName = do
  contents <- BC.readFile fileName
  let vals = parseContents region contents
  let arrBounds = ((regFirstLine region, 0), (regLastLine region, 5999))
  return . Topo $ listArray arrBounds (concat vals)

parseContents :: FileRegion -> BC.ByteString -> [[Int]]
parseContents region s =
    let rest = parseHeader (BC.lines s) in
    parseLines region rest

parseHeader :: [BC.ByteString] -> [BC.ByteString]
parseHeader = drop 6

parseLines :: FileRegion -> [BC.ByteString] -> [[Int]]
parseLines region =
    map parseLine . take (regNumLines region) . drop (regFirstLine region)

parseLine :: BC.ByteString -> [Int]
parseLine = map (read . BC.unpack) . BC.words

toPixels :: Arr -> [Word8]
toPixels a = concat (mapArrToLists heightToPix a)

mapArrToLists :: (Int -> a) -> Arr -> [[a]]
mapArrToLists f a = map (map f) lists
    where lists = map (map (a !)) (coords a)

coords :: Arr -> [[(Int, Int)]]
coords a = [[(y, x) | x <- [minx..maxx]] | y <- [miny..maxy]]
    where ((miny, minx), (maxy, maxx)) = bounds a

heightToPix :: Int -> Word8
heightToPix x | x < 0 = 0
              | x >= 0 && x <= 8*128 = 127 + fromIntegral (x `div` 8)
              | otherwise = 255

toPGM :: Arr -> B.ByteString
toPGM arr = B.concat [BC.pack header, B.pack (toPixels arr)]
    where header = concat [ "P5\n",
                            show width, " ", show height, "\n",
                            "255\n" ]
          width = (maxx - minx) + 1
          height = (maxy - miny) + 1
          ((miny, minx), (maxy, maxx)) = bounds arr

writePGM :: String -> Topo -> IO ()
writePGM fileName (Topo arr) = B.writeFile fileName (toPGM arr)

type Heights = Array (Int, Int) (Double, Double, Double)

topoHeights :: (Int, Int) -> (Int, Int) -> Topo -> Heights
topoHeights (minLine, minSamp) (lineCount, sampCount) (Topo arr) =
    arrayFromFn bnds pointAt
    where
      bnds = ( (minLine, minSamp),
               (minLine + lineCount - 1, minSamp + sampCount - 1) )
      pointAt (y, x) = (fromIntegral (x-minSamp) * 60, -- TODO
                        fromIntegral (y-minLine) * 60, -- TODO
                        heightFromVal (arr ! (y, x)))

arrayFromFn :: Ix ix => (ix, ix) -> (ix -> a) -> Array ix a
arrayFromFn bnds f = listArray bnds (map f (range bnds))

heightFromVal :: Int -> Double
heightFromVal (-9999) = 0
heightFromVal x = fromIntegral x
