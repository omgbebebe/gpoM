module Main where

import GpoM.FS
import System.Directory
import System.FilePath

main :: IO ()
main = do
  putStrLn "Starting GpoM..."
  cwd <- getCurrentDirectory
  let hmp = cwd </> ".hadfs"
      smp = cwd </> ".sysvol"
      mp = cwd </> "mnt"
      cmp = cwd </> ".cache"
  runFS hmp smp mp cmp print
