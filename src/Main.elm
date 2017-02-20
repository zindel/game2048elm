module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Keyboard
import List
import Debug
import AnimationFrame
import Animation exposing (Animation, animate, animation, from, to, static, duration, isDone, getTo)
import Time exposing (Time, second)
import Window
import Task
import Random


-- MODEL


type BoardState
    = AwaitingKeyPress
    | MovingTiles
    | CollapsingEquals
    | GettingNewNumber


type Direction
    = Left
    | Up
    | Right
    | Down


type alias TileData =
    { row : Int
    , col : Int
    , value : Int
    }


type alias Tile =
    { row : Animation
    , col : Animation
    , size : Animation
    , value : Int
    }


type alias Board =
    List Tile


type Msg
    = Init ( Window.Size, Time )
    | Move Direction
    | Collapse
    | AddRandomTile
    | AddTile TileData
    | Tick Time
    | NoOp


type alias Model =
    { initialBoard : List TileData
    , board : Board
    , time : Time
    , boardState : BoardState
    , nextMsg : Msg
    }


addTile : Model -> TileData -> Model
addTile model { row, col, value } =
    let
        tile =
            Tile
                (toFloat row |> static)
                (toFloat col |> static)
                (sizeAnimation model.time)
                value
    in
        { model | board = model.board ++ [ tile ] }


createTile : Time -> Int -> Int -> Int -> Tile
createTile time row col value =
    Tile
        (toFloat row |> static)
        (toFloat col |> static)
        (sizeAnimation time)
        value


init : List TileData -> Model
init initialBoard =
    { initialBoard = initialBoard
    , board = []
    , boardState = AwaitingKeyPress
    , time = 0
    , nextMsg = NoOp
    }


isBoardReady : Board -> Bool
isBoardReady board =
    case board of
        [] ->
            False

        tile :: [] ->
            tile.value > 4

        _ ->
            True


sizeAnimation : Time -> Animation
sizeAnimation time =
    let
        start =
            cellWidth - 6
    in
        animation time |> from start |> to cellWidth |> duration (0.2 * second)


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
                    |> duration (0.2 * second)
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


moveBoard : Direction -> Model -> Model
moveBoard direction model =
    let
        ( changing, sortDirection ) =
            case direction of
                Left ->
                    ( "col", 1 )

                Up ->
                    ( "row", 1 )

                Right ->
                    ( "col", -1 )

                Down ->
                    ( "row", -1 )

        ( same, get, set ) =
            if changing == "col" then
                ( .row >> getTo >> floor
                , .col >> getTo >> floor
                , \t v -> moveTile model.time (same t) v t
                )
            else
                ( .col >> getTo >> floor
                , .row >> getTo >> floor
                , \t v -> moveTile model.time v (same t) t
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
        { model | board = moveTiles model.board, nextMsg = Collapse }


findTiles : ( Int, Int ) -> Board -> List Tile
findTiles ( row, col ) =
    List.filter
        (\t ->
            (getTo t.row) == (toFloat row) && (getTo t.col) == (toFloat col)
        )


collapseBoard : Model -> Model
collapseBoard model =
    let
        filterTiles tiles =
            case tiles of
                t1 :: t2 :: [] ->
                    let
                        nextTile =
                            resizeTile model.time t1
                    in
                        [ { nextTile | value = t1.value * 2 } ]

                _ ->
                    tiles

        nextBoard =
            cells
                |> List.map (\cell -> (findTiles cell >> filterTiles) model.board)
                |> List.concat
    in
        { model | board = nextBoard, nextMsg = AddRandomTile }


emptyCells : Board -> List ( Int, Int )
emptyCells board =
    cells |> List.filter (\cell -> (findTiles cell board |> List.length) == 0)



-- addTile : Time -> ( Int, Int ) -> Board -> Board
-- addTile time ( row, col ) board =
--     board ++ [ createTile time row col 2 ]
-- UPDATE


initCmd : Cmd Msg
initCmd =
    Task.map2 (,) Window.size Time.now
        |> Task.perform Init


addRandomTileCmd : Model -> Cmd Msg
addRandomTileCmd model =
    let
        empty =
            emptyCells model.board

        takeCell n =
            List.drop (n - 1) empty |> List.head |> Maybe.withDefault ( 0, 0 )

        generator =
            Random.map
                (\n ->
                    let
                        ( row, col ) =
                            takeCell n
                    in
                        TileData row col 2
                )
                (Random.int 1 (List.length empty))
    in
        Random.generate AddTile generator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        addRandomTileOnInitCmd model =
            if isBoardReady model.board then
                Cmd.none
            else
                addRandomTileCmd model

        initModel initialBoard model =
            case initialBoard of
                [] ->
                    model

                tileData :: tail ->
                    addTile model tileData |> initModel tail
    in
        case msg of
            Init ( size, time ) ->
                let
                    nextModel =
                        initModel model.initialBoard ({ model | time = time })
                in
                    nextModel ! [ addRandomTileOnInitCmd nextModel ]

            AddTile tileData ->
                let
                    nextModel =
                        addTile model tileData
                in
                    { nextModel | nextMsg = NoOp }
                        ! [ addRandomTileOnInitCmd nextModel ]

            Move direction ->
                moveBoard direction model ! []

            Collapse ->
                collapseBoard model ! []

            AddRandomTile ->
                model ! [ addRandomTileCmd model ]

            Tick time ->
                let
                    nextModel =
                        { model | time = time }
                in
                    if isAnimationRunning model then
                        nextModel ! []
                    else
                        update model.nextMsg nextModel

            NoOp ->
                model ! []



-- VIEW


boardSize : number
boardSize =
    4


cellWidth : number
cellWidth =
    80


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
                        || not (isDone time tile.size)
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
        , Html.text (List.length model.board |> toString)
        , Html.text (" currentState " ++ toString model.boardState)
        , Html.text (" isAnimationRunning " ++ toString (isAnimationRunning model))
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        direction keyCode =
            case keyCode of
                37 ->
                    Move Left

                38 ->
                    Move Up

                39 ->
                    Move Right

                40 ->
                    Move Down

                _ ->
                    NoOp
    in
        if isBoardReady model.board then
            Sub.batch
                [ if model.nextMsg == NoOp then
                    Sub.map direction (Keyboard.downs identity)
                  else
                    Sub.none
                , AnimationFrame.times Tick
                ]
        else
            Sub.none



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
    let
        initialModel =
            init []

        -- [ TileData 1 1 2
        -- , TileData 1 2 2
        -- , TileData 1 3 4
        -- ]
    in
        Html.program
            { init = ( initialModel, initCmd )
            , subscriptions = subscriptions
            , update = update
            , view = view
            }
