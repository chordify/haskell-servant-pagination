module Color where

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)

import qualified Data.Aeson   as Aeson


data Color = Color
  { name :: String
  , rgb  :: [Int]
  , hex  :: String
  } deriving (Eq, Show, Generic)


instance ToJSON Color where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions


colors :: [Color]
colors =
  [ Color "Aqua" [0, 255, 255] "#00ffff"
  , Color "Black" [0, 0, 0] "#000000"
  , Color "Blue" [0, 0, 255] "#0000ff"
  , Color "BlueViolet" [95, 0, 255] "#5f00ff"
  , Color "CadetBlue" [95, 175, 135] "#5faf87"
  , Color "CadetBlue" [95, 175, 175] "#5fafaf"
  , Color "CornflowerBlue" [95, 135, 255] "#5f87ff"
  , Color "DarkBlue" [0, 0, 135] "#000087"
  , Color "DarkCyan" [0, 175, 135] "#00af87"
  , Color "DarkGoldenrod" [175, 135, 0] "#af8700"
  , Color "DarkGreen" [0, 95, 0] "#005f00"
  , Color "DarkKhaki" [175, 175, 95] "#afaf5f"
  , Color "DarkMagenta" [135, 0, 135] "#870087"
  , Color "DarkMagenta" [135, 0, 175] "#8700af"
  , Color "DarkOrange" [255, 135, 0] "#ff8700"
  , Color "DarkRed" [135, 0, 0] "#870000"
  , Color "DarkRed" [95, 0, 0] "#5f0000"
  , Color "DarkSeaGreen" [135, 175, 135] "#87af87"
  , Color "DarkTurquoise" [0, 215, 215] "#00d7d7"
  , Color "DarkViolet" [135, 0, 215] "#8700d7"
  , Color "DarkViolet" [175, 0, 215] "#af00d7"
  , Color "Fuchsia" [255, 0, 255] "#ff00ff"
  , Color "Green" [0, 128, 0] "#008000"
  , Color "GreenYellow" [175, 255, 0] "#afff00"
  , Color "Grey" [128, 128, 128] "#808080"
  , Color "HotPink" [255, 95, 175] "#ff5faf"
  , Color "HotPink" [255, 95, 215] "#ff5fd7"
  , Color "IndianRed" [175, 95, 95] "#af5f5f"
  , Color "IndianRed" [215, 95, 95] "#d75f5f"
  , Color "LightCoral" [255, 135, 135] "#ff8787"
  , Color "LightGreen" [135, 255, 135] "#87ff87"
  , Color "LightGreen" [135, 255, 95] "#87ff5f"
  , Color "LightSeaGreen" [0, 175, 175] "#00afaf"
  , Color "LightSlateBlue" [135, 135, 255] "#8787ff"
  , Color "LightSlateGrey" [135, 135, 175] "#8787af"
  , Color "LightSteelBlue" [175, 175, 255] "#afafff"
  , Color "Lime" [0, 255, 0] "#00ff00"
  , Color "Maroon" [128, 0, 0] "#800000"
  , Color "MediumOrchid" [175, 95, 215] "#af5fd7"
  , Color "MediumPurple" [135, 135, 215] "#8787d7"
  , Color "MediumSpringGreen" [0, 255, 175] "#00ffaf"
  , Color "MediumTurquoise" [95, 215, 215] "#5fd7d7"
  , Color "MediumVioletRed" [175, 0, 135] "#af0087"
  , Color "Navy" [0, 0, 128] "#000080"
  , Color "NavyBlue" [0, 0, 95] "#00005f"
  , Color "Olive" [128, 128, 0] "#808000"
  , Color "Orchid" [215, 95, 215] "#d75fd7"
  , Color "Purple" [128, 0, 128] "#800080"
  , Color "Purple" [135, 0, 255] "#8700ff"
  , Color "Purple" [175, 0, 255] "#af00ff"
  , Color "Red" [255, 0, 0] "#ff0000"
  , Color "RosyBrown" [175, 135, 135] "#af8787"
  , Color "SandyBrown" [255, 175, 95] "#ffaf5f"
  , Color "Silver" [192, 192, 192] "#c0c0c0"
  , Color "Tan" [215, 175, 135] "#d7af87"
  , Color "Teal" [0, 128, 128] "#008080"
  , Color "Violet" [215, 135, 255] "#d787ff"
  , Color "White" [255, 255, 255] "#ffffff"
  , Color "Yellow" [255, 255, 0] "#ffff00"
  ]
