module Game2048.Model exposing (..)

import Game2048.Util exposing (cells)
import Animation exposing (Animation, getTo, animation, from, to, static, duration)
import Time exposing (Time, second)
import Window
import Random


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
    = Init Time
    | Resize Window.Size
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
    , nextMsg : Msg
    , width : Float
    , left : Float
    , cellWidth : Float
    , borderWidth : Float
    }


boardSize : number
boardSize =
    4


addTile : Model -> TileData -> Model
addTile model { row, col, value } =
    let
        tile =
            Tile
                (toFloat row |> static)
                (toFloat col |> static)
                (sizeAnimation model.cellWidth model.time)
                value
    in
        { model | board = model.board ++ [ tile ] }


init : List TileData -> Model
init initialBoard =
    { initialBoard = initialBoard
    , board = []
    , time = 0
    , nextMsg = NoOp
    , width = 0
    , left = 0
    , cellWidth = 0
    , borderWidth = 0
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


sizeAnimation : Float -> Time -> Animation
sizeAnimation cellWidth time =
    animation time
        |> from (cellWidth * 0.75)
        |> to cellWidth
        |> duration (0.1 * second)


resizeTile : Float -> Time -> Tile -> Tile
resizeTile cellWidth time tile =
    { tile | size = sizeAnimation cellWidth time }


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
                    |> duration (0.12 * second)
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


move : Direction -> Model -> Model
move direction model =
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


collapse : Model -> Model
collapse model =
    let
        filterTiles tiles =
            case tiles of
                t1 :: t2 :: [] ->
                    let
                        nextTile =
                            resizeTile model.cellWidth model.time t1
                    in
                        [ { nextTile | value = t1.value * 2 } ]

                _ ->
                    tiles

        nextBoard =
            cells boardSize
                |> List.map (\cell -> (findTiles cell >> filterTiles) model.board)
                |> List.concat
    in
        { model | board = nextBoard, nextMsg = AddRandomTile }


emptyCells : Board -> List ( Int, Int )
emptyCells board =
    cells boardSize
        |> List.filter (\cell -> (findTiles cell board |> List.length) == 0)


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


resize : Window.Size -> Model -> Model
resize { width, height } model =
    let
        gameWidth =
            toFloat height
                * 0.7
                |> min (toFloat width * 0.6)
                |> max 300.0

        borderWidth =
            gameWidth * 0.1 / (boardSize + 1)

        gameLeft =
            toFloat width / 2.0 - gameWidth / 2 |> max 0

        cellWidth =
            (gameWidth - borderWidth * (boardSize + 1)) / boardSize
    in
        { model
            | width = gameWidth
            , left = gameLeft
            , cellWidth = cellWidth
            , borderWidth = borderWidth
            , board =
                model.board
                    |> List.map (resizeTile cellWidth model.time)
        }

isRunning : Model -> Bool
isRunning model =
    isBoardReady model.board || model.nextMsg /= NoOp
