module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard
import List
import Debug
import AnimationFrame
import Animation exposing (Animation, animate, animation, from, to, static, duration, isDone, getTo)
import Time exposing (Time, second)
import Tuple exposing (mapFirst, mapSecond)


type Msg
    = ArrowKey Int
    | Tick Time



-- MODEL


type alias Direction =
    Int


type alias Tile =
    { row : Animation
    , col : Animation
    , value : Int
    }


type alias Board =
    List Tile


type alias Model =
    { board : Board
    , time : Time
    }


createTile : Int -> Int -> Int -> Tile
createTile row col value =
    Tile (toFloat row |> static) (toFloat col |> static) value


init : ( Model, Cmd Msg )
init =
    ( { board =
            [ createTile 1 1 2
            , createTile 1 2 2
            ]
      , time = 0
      }
    , Cmd.none
    )


moveTile : Time -> Int -> Int -> Tile -> Tile
moveTile time row col tile =
    let
        move start end =
            if start == end then
                static start
            else
                animation time
                    |> from start
                    |> to end
                    |> duration (0.3 * second)
    in
        { tile
            | row = move (getTo tile.row) (toFloat row)
            , col = move (getTo tile.col) (toFloat col)
        }


collapse : Int -> (Int -> Int) -> List ( Int, Int ) -> List Int
collapse startPos nextPos positions =
    let
        acc =
            ( startPos, 0, False )

        newPos ( pos, value ) ( lastPos, lastValue, collapsed ) =
            if (value == lastValue) && (not collapsed) then
                ( lastPos, lastValue, True )
            else
                ( nextPos lastPos, value, False )
    in
        positions
            |> List.scanl newPos acc
            |> List.map (\( f, _, _ ) -> f)
            |> List.drop 1


moveBoard : Time -> Direction -> Board -> Board
moveBoard time direction board =
    let
        ( changing, sortDirection ) =
            case direction of
                37 ->
                    ( "col", 1 )

                38 ->
                    ( "row", 1 )

                39 ->
                    ( "col", -1 )

                40 ->
                    ( "row", -1 )

                _ ->
                    ( "", 0 )

        ( same, get, set ) =
            if changing == "col" then
                ( .row >> getTo >> floor
                , .col >> getTo >> floor
                , \t v -> moveTile time (same t) v t
                )
            else
                ( .col >> getTo >> floor
                , .row >> getTo >> floor
                , \t v -> moveTile time v (same t) t
                )

        ( startPos, nextPos ) =
            if sortDirection == 1 then
                ( 0, (+) 1 )
            else
                ( boardSize + 1, (+) -1 )

        sortFn tile =
            (get tile) * sortDirection

        posValue tile =
            ( get tile, tile.value )

        _ =
            nextPos startPos

        moveTiles tiles =
            -- iterate over rows or columns (what is not changing)
            List.range 1 boardSize
                -- split list into chunks where row or column is the same
                |>
                    List.map (\s -> List.filter (\t -> same t == s) tiles)
                -- sort each chunk
                |>
                    List.map (\chunk -> List.sortBy sortFn chunk)
                -- produce the tuple of (chunk, target positions)
                -- for each chunk
                |>
                    List.map
                        (\chunk ->
                            ( chunk
                            , chunk
                                |> List.map posValue
                                |> collapse startPos nextPos
                            )
                        )
                -- finally, move each tile to the target position
                |>
                    List.map
                        (\( chunk, positions ) ->
                            List.map2 set chunk positions
                        )
                |> List.concat
    in
        if sortDirection /= 0 then
            moveTiles board
        else
            board



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowKey code ->
            ( { model
                | board = moveBoard model.time code model.board
              }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }, Cmd.none )



-- VIEW


boardSize : number
boardSize =
    4


cellWidth : number
cellWidth =
    40


borderWidth : number
borderWidth =
    5


isMoving : Model -> Bool
isMoving model =
    let
        isMoving_ time tiles =
            case tiles of
                [] ->
                    False

                tile :: tail ->
                    not (isDone time tile.row)
                        || not (isDone time tile.col)
                        || isMoving_ time tail
    in
        isMoving_ model.time model.board


toTopLeft : ( number, number ) -> ( Float, Float )
toTopLeft ( row, col ) =
    ( borderWidth * row + (row - 1) * cellWidth
    , borderWidth * col + (col - 1) * cellWidth
    )


viewBox : Float -> Float -> String -> String -> Html Msg
viewBox left top color text =
    Html.div
        [ style
            [ ( "position", "absolute" )
            , ( "left", toString left ++ "px" )
            , ( "top", toString top ++ "px" )
            , ( "width", toString cellWidth ++ "px" )
            , ( "height", toString cellWidth ++ "px" )
            , ( "backgroundColor", color )
            , ( "textAlign", "center" )
            , ( "verticalAlign", "middle" )
            ]
        ]
        [ Html.text text ]


viewCell : ( Int, Int ) -> Html Msg
viewCell ( row, col ) =
    let
        ( top, left ) =
            toTopLeft ( row, col )
    in
        viewBox left top "gray" ""


viewCells : List (Html Msg)
viewCells =
    let
        range =
            List.range 1 boardSize
    in
        range
            |> List.map (\x -> List.map ((,) x) range)
            |> List.concat
            |> List.map viewCell


viewTile : Time -> Tile -> Html Msg
viewTile time tile =
    let
        ( top, left ) =
            toTopLeft ( animate time tile.row, animate time tile.col )
    in
        viewBox left top "blue" (toString tile.value)


viewBoard : Time -> Board -> Html Msg
viewBoard time board =
    let
        boardSizePx =
            cellWidth * boardSize + (borderWidth * (boardSize + 1))

        viewTiles =
            List.map (viewTile time) board
    in
        viewCells
            ++ viewTiles
            |> Html.div
                [ style
                    [ ( "position", "relative" )
                    , ( "width", toString boardSizePx ++ "px" )
                    , ( "height", toString boardSizePx ++ "px" )
                    , ( "backgroundColor", "green" )
                    ]
                ]


view : Model -> Html Msg
view model =
    Html.div
        [ style [ ( "position", "relative" ) ] ]
        [ viewBoard model.time model.board
        , Html.text (toString model)
        , Html.text (" isMoving " ++ toString (isMoving model))
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if isMoving model then
            Sub.none
          else
            Keyboard.downs ArrowKey
        , AnimationFrame.times Tick
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
