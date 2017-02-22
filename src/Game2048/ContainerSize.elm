port module Game2048.Ports exposing(size)

import Game2048.Model exposing (Size, TileData)

port size : (Size -> msg) -> Sub msg

port save : (Int, List TileData) -> Cmd msg
