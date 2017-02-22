port module Game2048.Ports exposing (size, save)

import Game2048.Model exposing (Size, TileData)


port size : (Size -> msg) -> Sub msg


port save : (Int,  Int, List TileData ) -> Cmd msg
