module Game2048.View exposing (..)

import Game2048.Model exposing (Model, Msg, Tile, boardSize)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Time exposing (Time)
import Animation exposing (animate)

import Game2048.Util exposing (cells)

toTopLeft : Float -> Float -> Float -> Float -> Float -> ( Float, Float )
toTopLeft borderWidth cellWidth row col size =
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
            , ( "line-height", toString size ++ "px" )
            , ( "backgroundColor", color )
            , ( "textAlign", "center" )
            , ( "verticalAlign", "middle" )
            ]
        ]
        [ Html.text text ]


viewCell : Float -> Float -> ( Int, Int ) -> Html Msg
viewCell borderWidth cellWidth ( row, col ) =
    let
        ( top, left ) =
            toTopLeft borderWidth cellWidth (toFloat row) (toFloat col) cellWidth
    in
        viewBox left top cellWidth "gray" ""


viewCells : Float -> Float -> List (Html Msg)
viewCells borderWidth cellWidth =
    cells boardSize |> List.map (viewCell borderWidth cellWidth)


viewTile : Float -> Float -> Time -> Tile -> Html Msg
viewTile borderWidth cellWidth time tile =
    let
        size =
            animate time tile.size

        ( top, left ) =
            toTopLeft borderWidth cellWidth (animate time tile.row) (animate time tile.col) size
    in
        viewBox left top size "blue" (toString tile.value)


viewBoard : Model -> Html Msg
viewBoard model =
    let
        {width, borderWidth, cellWidth} = model.layout
        viewTiles =
            List.map (viewTile borderWidth cellWidth model.time) model.board
    in
        viewCells borderWidth cellWidth
            ++ viewTiles
            |> Html.div
                [ style
                    [ ( "position", "absolute" )
                    , ( "left", "0px" )
                    , ( "top", "0px" )
                    , ( "width", toString width ++ "px" )
                    , ( "height", toString width ++ "px" )
                    , ( "backgroundColor", "green" )
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
            , ( "backgroundColor", "yellow" )
            ]
        ]
        [ viewBoard model
        , Html.text (List.length model.board |> toString)
        ]
