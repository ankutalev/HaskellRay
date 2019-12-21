module Ray where
import Commons.Utils

data LightPoint = LightPoint {p :: Point Double, c::Color Int} deriving (Show,Eq)