module Scene.SceneParser where 

import System.IO
import Text.Read
import Data.Maybe
import Data.Char (isSpace)
import Data.List
import Primitives.Sphere
import Commons.Utils
import Ray
import Scene.Scene



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
parser::(FromList a)=>[String]->(Double->Bool)->Maybe a
parser x f | any (==Nothing) readed = Nothing
           | otherwise = fromList $ filter f $ fromJust <$> readed 
            where 
                 readed::[Maybe Double]
                 readed = readMaybe <$> x

parseColor::[String]->Maybe (Color Int)
parseColor arg = parser arg (\x-> x>=0 && x<= 255 )
                  
parsePoint::[String]->Maybe (Point Double)
parsePoint arg = parser arg (\x->True)

        
parseLightPoints :: [String] -> Maybe LightPoint    
parseLightPoints arg = do
                         let (p,c) = splitAt 3 arg
                         p <- parsePoint p
                         c <- parseColor c
                         return $ LightPoint p c 
                         
parseObjects::String ->Maybe Scene
parseObjects content = do
                         let (diffuse:n:body) =  filter (\x->x /="" && not ("//" `isPrefixOf` x)) $ trim <$> lines content
                         diffusion <- parseColor . words $ diffuse
                         let (lp,primitives) = splitAt (read n) body
                         lightPoints <- mapM parseLightPoints ( words <$> lp)
                         return $ Scene diffusion lightPoints
                         
parseScene::String->IO (Maybe Scene)
parseScene fileName = withFile fileName ReadMode (\h -> do
                                                          content <- hGetContents h
                                                          let scene = parseObjects content
                                                          print scene
                                                          return scene
                                                  )
                                                                                                    