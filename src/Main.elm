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


-- MODEL


type BoardState
    = AwaitingKeyPress
    | MovingTiles
    | CollapsingEquals


type alias Direction =
    Int


type alias Tile =
    { row : Animation
    , col : Animation
    , size : Animation
    , value : Int
    }


type alias Board =
    List Tile


type alias Model =
    { board : Board
    , time : Time
    , boardState : BoardState
    }


createTile : Int -> Int -> Int -> Tile
createTile row col value =
    Tile
        (toFloat row |> static)
        (toFloat col |> static)
        (static cellWidth)
        value


init : ( Model, Cmd Msg )
init =
    ( { board =
            [ createTile 1 1 2
            , createTile 1 2 2
            , createTile 1 3 4
            ]
      , boardState = AwaitingKeyPress
      , time = 0
      }
    , Cmd.none
    )


sizeAnimation : Time -> Animation
sizeAnimation time =
    let
        start =
            cellWidth - 6
    in
        animation time |> from start |> to cellWidth |> duration (0.3 * second)


resizeTile : Time -> Tile -> Tile
resizeTile time tile =
    { tile | size = sizeAnimation time }


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


newPositions : Int -> (Int -> Int) -> List ( Int, Int ) -> List Int
newPositions startPos nextPos positions =
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

        startPos =
            if sortDirection == 1 then
                0
            else
                boardSize + 1

        sortFn tile =
            (get tile) * sortDirection

        posValue tile =
            ( get tile, tile.value )

        moveTiles tiles =
            -- iterate over rows or columns (what is not changing)
            range
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
                                |> newPositions startPos ((+) sortDirection)
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


findTiles : ( Int, Int ) -> Board -> List Tile
findTiles ( row, col ) =
    List.filter
        (\t ->
            (getTo t.row) == (toFloat row) && (getTo t.col) == (toFloat col)
        )


collapseBoard : Time -> Board -> Board
collapseBoard time board =
    let
        filterTiles tiles =
            case tiles of
                t1 :: t2 :: [] ->
                    let
                        nextTile =
                            resizeTile time t1
                    in
                        [ { nextTile | value = t1.value * 2 } ]

                _ ->
                    tiles
    in
        cells
            |> List.map (\cell -> (findTiles cell >> filterTiles) board)
            |> List.concat



-- UPDATE


type Msg
    = ArrowKey Int
    | Tick Time


updateTick : Time -> Model -> ( Model, Cmd Msg )
updateTick time model =
    let
        waitForAnimation model nextBoardState =
            if isAnimationRunning model then
                model
            else
                { model | boardState = nextBoardState }

        nextModel =
            case model.boardState of
                AwaitingKeyPress ->
                    model

                MovingTiles ->
                    if isAnimationRunning model then
                        model
                    else
                        { model
                            | boardState = CollapsingEquals
                            , board = collapseBoard model.time model.board
                        }

                CollapsingEquals ->
                    waitForAnimation model AwaitingKeyPress
    in
        ( { nextModel | time = time }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowKey code ->
            ( { model
                | board = moveBoard model.time code model.board
                , boardState = MovingTiles
              }
            , Cmd.none
            )

        Tick time ->
            updateTick time model



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


isAnimationRunning : Model -> Bool
isAnimationRunning model =
    let
        isBoardAnimationRunning_ time tiles =
            case tiles of
                [] ->
                    False

                tile :: tail ->
                    not (isDone time tile.row)
                        || not (isDone time tile.col)
                        || isBoardAnimationRunning_ time tail
    in
        isBoardAnimationRunning_ model.time model.board


toTopLeft : Float -> Float -> Float -> ( Float, Float )
toTopLeft row col size =
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


viewCell : ( Int, Int ) -> Html Msg
viewCell ( row, col ) =
    let
        ( top, left ) =
            toTopLeft (toFloat row) (toFloat col) cellWidth
    in
        viewBox left top cellWidth "gray" ""


viewCells : List (Html Msg)
viewCells =
    cells |> List.map viewCell


viewTile : Time -> Tile -> Html Msg
viewTile time tile =
    let
        size =
            animate time tile.size

        ( top, left ) =
            toTopLeft (animate time tile.row) (animate time tile.col) size
    in
        viewBox left top size "blue" (toString tile.value)


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
        , Html.text (" isAnimationRunning " ++ toString (isAnimationRunning model))
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.boardState == AwaitingKeyPress then
            Keyboard.downs ArrowKey
          else
            Sub.none
        , AnimationFrame.times Tick
        ]



-- UTILS


range : List Int
range =
    List.range 1 boardSize


cells : List ( Int, Int )
cells =
    range
        |> List.map (\x -> List.map ((,) x) range)
        |> List.concat



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
