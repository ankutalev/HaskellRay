module Main where

--import Lib
import SceneParser

main = do
          scene <- parseScene "../examples/bunny.scene"
          print scene
