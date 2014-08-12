module CGIAR (filesForArea) where

import Area
import LatLong

filesForArea :: Area -> Maybe [FilePath]
filesForArea area = fmap (map nameForNums) (numsForArea area)

numsForArea :: Area -> Maybe [(Int, Int)]
numsForArea area = do
  (sNum, wNum) <- boxNums (areaSW area)
  (nNum, eNum) <- boxNums (areaNE area)
  return [(y, x) | y <- [sNum..nNum], x <- [wNum..eNum]]

boxNums :: LatLong -> Maybe (Int, Int)
boxNums pos = if valid then Just (lat1To24, long1To72) else Nothing
    where (lat, long) = latLongToSecs pos
          latNeg12To11 = lat `div` 3600 `div` 5
          longNeg36To35 = long `div` 3600 `div` 5
          lat1To24 = 12 - latNeg12To11
          long1To72 = longNeg36To35 + 37
          valid = 1 <= lat1To24 && lat1To24 <= 24
                  && 1 <= long1To72 && long1To72 <= 72

nameForNums :: (Int, Int) -> FilePath
nameForNums (y, x) = "srtm_" ++ show02 x ++ "_" ++ show02 y ++ ".asc"

show02 :: Int -> String
show02 x | x < 0 = error "show02: negative number"
         | x < 10 = '0':show x
         | otherwise = show x
