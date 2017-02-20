module Game2048.Util exposing (..)


cells : Int -> List ( Int, Int )
cells size =
    let
        range =
            List.range 1 size
    in
        range
            |> List.map (\x -> List.map ((,) x) range)
            |> List.concat
