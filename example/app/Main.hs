module Main (main) where

import Katip.Wai.Example


main :: IO ()
main =
  Katip.Wai.Example.servant 4000
