module Main where

--import Lib
import Scene.SceneParser

main = do
          scene <- parseScene "../examples/bunny.scene"
          print scene
