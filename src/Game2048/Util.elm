module Game2048.Util exposing (..)

import Color exposing (Color)


cells : Int -> List ( Int, Int )
cells size =
    let
        range =
            List.range 1 size
    in
        range
            |> List.map (\x -> List.map ((,) x) range)
            |> List.concat


colorToCSS : Color -> String
colorToCSS color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba("
            ++ toString red
            ++ ","
            ++ toString green
            ++ ","
            ++ toString blue
            ++ ","
            ++ toString alpha
            ++ ")"
