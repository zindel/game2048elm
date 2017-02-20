module Game2048.View exposing (..)

import Game2048.Model exposing (Model, Msg, Tile, boardSize)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Time exposing (Time)
import Animation exposing (animate)

import Game2048.Util exposing (cells)

borderWidth : number
borderWidth =
    5

toTopLeft : Float -> Float -> Float -> Float -> ( Float, Float )
toTopLeft cellWidth row col size =
    let
        center =
            (cellWidth - size) / 2.0
    in
        ( center + borderWidth * row + (row - 1) * cellWidth
        , center + borderWidth * col + (col - 1) * cellWidth
        )


viewBox : Float -> Float -> Float -> String -> String -> Html Msg
viewBox left top size color text =
    Html.div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString left ++ "px" )
            , ( "top", toString top ++ "px" )
            , ( "width", toString size ++ "px" )
            , ( "height", toString size ++ "px" )
            , ( "backgroundColor", color )
            , ( "textAlign", "center" )
            , ( "verticalAlign", "middle" )
            ]
        ]
        [ Html.text text ]


viewCell : Float -> ( Int, Int ) -> Html Msg
viewCell cellWidth ( row, col ) =
    let
        ( top, left ) =
            toTopLeft cellWidth (toFloat row) (toFloat col) cellWidth
    in
        viewBox left top cellWidth "gray" ""


viewCells : Float -> List (Html Msg)
viewCells cellWidth =
    cells boardSize |> List.map (viewCell cellWidth)


viewTile : Float -> Time -> Tile -> Html Msg
viewTile cellWidth time tile =
    let
        size =
            animate time tile.size

        ( top, left ) =
            toTopLeft cellWidth (animate time tile.row) (animate time tile.col) size
    in
        viewBox left top size "blue" (toString tile.value)


viewBoard : Model -> Html Msg
viewBoard model =
    let
        viewTiles =
            List.map (viewTile model.cellWidth model.time) model.board
    in
        viewCells model.cellWidth
            ++ viewTiles
            |> Html.div
                [ style
                    [ ( "position", "absolute" )
                    , ( "left", "0px" )
                    , ( "top", "0px" )
                    , ( "width", toString model.width ++ "px" )
                    , ( "height", toString model.width ++ "px" )
                    , ( "backgroundColor", "green" )
                    ]
                ]


view : Model -> Html Msg
view model =
    Html.div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString model.left ++ "px" )
            , ( "top", "0px" )
            , ( "bottom", "0px" )
            , ( "width", toString model.width ++ "px" )
            , ( "backgroundColor", "yellow" )
            ]
        ]
        [ viewBoard model
        , Html.text (List.length model.board |> toString)
        ]
