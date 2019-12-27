module Scene.Scene where
import Commons.Utils
import Ray

data Scene = Scene { diffusion::Color Int,
					 lightPoints::[LightPoint]
				   } deriving (Show, Eq)

defaultScene::Scene
defaultScene = Scene (RGBModel 0 0 0)  []				   