port module Game2048.Ports exposing(size)

import Game2048.Model exposing (Size)

port size : (Size -> msg) -> Sub msg
