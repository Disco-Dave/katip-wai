module Main (main) where

import Katip.Wai.Example qualified


main :: IO ()
main =
  Katip.Wai.Example.servant 4000
