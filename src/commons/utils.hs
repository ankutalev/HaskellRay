module Commons.Utils  where

class FromList a where
	fromList::[Double] -> Maybe a

data Point a = Point {x::a, y::a, z:: a} deriving (Show,Eq)
instance (Floating a)=> FromList (Point a)  where
	fromList [x,y,z] = Just $ Point (realToFrac x) (realToFrac y) (realToFrac z)    
	fromList _ = Nothing
	
data Color a = RGBModel {r::a, g::a, b::a} deriving (Show,Eq)
instance (Num a)=> FromList (Color a)  where
	fromList [r,g,b] = Just $ RGBModel (toA r) (toA g)  (toA b) where toA x = fromIntegral ( round x :: Int)  
	fromList _ = Nothing

instance Functor Color where
	fmap f  (RGBModel r g b) = RGBModel (f r) (f g) (f b)
	
toFloatModel::Color Int->Color Float
toFloatModel c = (/ 255) <$> fromIntegral <$> c


data OpticalCharacteristic a = Optical { diffusion::Color Double, mirror::Color Double, power::Double } deriving (Show,Eq) 