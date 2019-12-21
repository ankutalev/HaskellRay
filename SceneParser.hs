import System.IO
import Text.Read
import Data.Maybe

import Commons.Utils
import Ray
import Scene

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
						 
parseScene::String->IO Scene
parseScene fileName = withFile fileName ReadMode (\h -> do
															content <- hGetContents h
															let fileLines = lines content
															let x = parseColor . words $ head fileLines
															print x
															return defaultScene
												  )
						 