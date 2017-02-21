module Game2048.View exposing (..)

import Game2048.Model exposing (Model, Msg, Tile, Layout, boardSize)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Time exposing (Time)
import Animation exposing (animate)
import Color exposing (Color, rgba, rgb)
import Game2048.Util exposing (cells, colorToCSS)


toTopLeft : Layout -> Float -> Float -> Float -> ( Float, Float )
toTopLeft { borderWidth, cellWidth } row col size =
    let
        center =
            (cellWidth - size) / 2.0
    in
        ( center + borderWidth * row + (row - 1) * cellWidth
        , center + borderWidth * col + (col - 1) * cellWidth
        )


viewBox : Float -> Float -> Float -> Color -> Color -> String -> Html Msg
viewBox left top size color textColor text =
    let
        fontSize = size / (String.length text |> max 2 |> toFloat)
    in
    Html.div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString left ++ "px" )
            , ( "top", toString top ++ "px" )
            , ( "width", toString size ++ "px" )
            , ( "height", toString size ++ "px" )
            , ( "line-height", toString size ++ "px" )
            , ( "backgroundColor", colorToCSS color )
            , ( "color", colorToCSS textColor )
            , ( "fontWeight", "bold" )
            , ( "fontSize", toString fontSize  ++ "px")
            , ( "textAlign", "center" )
            , ( "verticalAlign", "middle" )
            ]
        ]
        [ Html.text text ]


viewCell : Layout -> ( Int, Int ) -> Html Msg
viewCell layout ( row, col ) =
    let
        ( top, left ) =
            toTopLeft layout (toFloat row) (toFloat col) layout.cellWidth
    in
        viewBox left top layout.cellWidth (rgba 238 228 218 0.35) Color.black ""


viewCells : Layout -> List (Html Msg)
viewCells layout =
    cells boardSize |> List.map (viewCell layout)


tileBackgroundColor : Int -> Color
tileBackgroundColor value =
    let
        color =
            [ rgb 238 228 218
            , rgb 237 224 200
            , rgb 242 177 121
            , rgb 245 149 99
            , rgb 246 124 95
            , rgb 246 94 59
            , rgb 237 207 114
            , rgb 237 204 97
            , rgb 237 200 80
            , rgb 237 197 63
            , rgb 237 194 46
            ]

        drop =
            logBase 2 (toFloat value) - 1 |> floor
    in
        color
            |> List.drop drop
            |> List.head
            |> Maybe.withDefault Color.black


tileTextColor : Int -> Color
tileTextColor value =
    if value < 8 then
        rgb 119 110 101
    else
        rgb 249 246 242


viewTile : Layout -> Time -> Tile -> Html Msg
viewTile layout time tile =
    let
        getValue =
            animate time

        ( top, left ) =
            toTopLeft layout
                (getValue tile.row)
                (getValue tile.col)
                (getValue tile.size)
    in
        viewBox left top (getValue tile.size) (tileBackgroundColor tile.value) (tileTextColor tile.value) (toString tile.value)


viewBoard : Model -> Html Msg
viewBoard { layout, time, board } =
    let
        viewTiles =
            List.map (viewTile layout time) board
    in
        viewCells layout
            ++ viewTiles
            |> Html.div
                [ style
                    [ ( "position", "absolute" )
                    , ( "left", "0px" )
                    , ( "top", "0px" )
                    , ( "width", toString layout.width ++ "px" )
                    , ( "height", toString layout.width ++ "px" )
                    , ( "backgroundColor", "#bbada0" )
                    ]
                ]


view : Model -> Html Msg
view model =
    Html.div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString model.layout.left ++ "px" )
            , ( "top", "0px" )
            , ( "bottom", "0px" )
            , ( "width", toString model.layout.width ++ "px" )
            ]
        ]
        [ viewBoard model
        , Html.text ((Color.rgb 10 10 10 |> toString) ++ "COLOR")
        ]
