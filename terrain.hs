module Main where

import qualified Stl
import qualified Model
import qualified Topo

main :: IO ()
main = do
  let region = Topo.FileRegion { Topo.regFirstLine = 4540, Topo.regNumLines = 100 }
  topo <- Topo.parseFile region "srtm_36_01.asc"
  -- writePGM "out.pgm" topo
  let hs = Topo.topoHeights (4540, 1280) (100, 200) topo
  let stl = Model.model hs
  writeFile "out.stl" (Stl.toString "topo" stl)
