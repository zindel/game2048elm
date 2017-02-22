module Game2048.Model
    exposing
        ( Model
        , Msg(..)
        , Direction(..)
        , TileData
        , Tile
        , Layout
        , ScoreUpdate
        , PopupType(..)
        , Popup
        , boardSize
        , init
        , collapse
        , move
        , canMove
        , canMoveTo
        , addTile
        , resize
        , isRunning
        , isAnimating
        , addRandomTileCmd
        , addRandomTileOnInitCmd
        , createPopup
        , clearScoreUpdate
        )

import Game2048.Util exposing (cells)
import Animation
    exposing
        ( Animation
        , isDone
        , getTo
        , animation
        , from
        , to
        , static
        , duration
        )
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
    | ShowPopup PopupType
    | NewGame
    | Continue
    | Tick Time
    | NoOp


type alias Layout =
    { width : Float
    , left : Float
    , cellWidth : Float
    , borderWidth : Float
    }


type alias ScoreUpdate =
    { value : Int
    , index : Int
    , animation : Animation
    }


type alias Model =
    { initialBoard : List TileData
    , board : Board
    , freePlay : Bool
    , score : Int
    , scoreUpdates : List ScoreUpdate
    , best : Int
    , time : Time
    , nextMsg : Msg
    , layout : Layout
    , popup : Maybe Popup
    }


type PopupType
    = Victory
    | GameOver


type alias Popup =
    { popupType : PopupType
    , opacity : Animation
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
                (sizeAnimation model.layout.cellWidth model.time)
                value
    in
        { model | board = model.board ++ [ tile ] }


init : Int -> List TileData -> Model
init best initialBoard =
    { initialBoard = initialBoard
    , board = []
    , freePlay = False
    , score = 0
    , scoreUpdates = []
    , best = best
    , time = 0
    , nextMsg = NoOp
    , layout = Layout 0 0 0 0
    , popup = Nothing
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


createPopup : PopupType -> Model -> Model
createPopup popupType model =
    { model
        | nextMsg = NoOp
        , popup =
            Just
                (animation model.time
                    |> from 0
                    |> to 0.6
                    |> duration (0.8 * second)
                    |> Popup popupType
                )
    }


sizeAnimation : Float -> Time -> Animation
sizeAnimation cellWidth time =
    animation time
        |> from (cellWidth * 0.5)
        |> to cellWidth
        |> duration (0.07 * second)


scoreAnimation : Time -> Animation
scoreAnimation time =
    animation time |> from 1 |> to -1 |> duration (0.5 * second)


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


canMove : Model -> Bool
canMove model =
    [ Left, Right, Up, Down ] |> List.any (\d -> canMoveTo d model)


canMoveTo : Direction -> Model -> Bool
canMoveTo direction model =
    let
        movedModel =
            move direction model
    in
        isAnimating movedModel


collapse : Model -> Model
collapse model =
    let
        filterTiles tiles =
            case tiles of
                t1 :: t2 :: [] ->
                    let
                        nextTile =
                            resizeTile model.layout.cellWidth model.time t1

                        value =
                            t1.value + t2.value
                    in
                        ( [ { nextTile | value = value } ], value )

                _ ->
                    ( tiles, 0 )

        boardWithScores =
            cells boardSize
                |> List.map (\cell -> (findTiles cell >> filterTiles) model.board)

        nextBoard =
            boardWithScores |> List.map Tuple.first |> List.concat

        scores =
            boardWithScores
                |> List.map Tuple.second
                |> List.filter ((<) 0)

        scoreSum =
            (List.sum scores)
    in
        updateScore (List.sum scores)
            { model
                | board = nextBoard
                , nextMsg =
                    if hasWinningTile nextBoard && not model.freePlay then
                        ShowPopup Victory
                    else
                        AddRandomTile
            }


updateScore : Int -> Model -> Model
updateScore scoreSum model =
    let
        nextScore =
            model.score + scoreSum

        scoreUpdates =
            if scoreSum > 0 then
                [ ScoreUpdate scoreSum
                    (List.length model.scoreUpdates)
                    (scoreAnimation model.time)
                ]
            else
                []
    in
        { model
            | score = nextScore
            , scoreUpdates = model.scoreUpdates ++ scoreUpdates
            , best =
                if nextScore > model.best then
                    nextScore
                else
                    model.best
        }


clearScoreUpdate : Model -> Model
clearScoreUpdate model =
    { model
        | scoreUpdates =
            List.filter
                (\{ animation } ->
                    not (isDone model.time animation)
                )
                model.scoreUpdates
    }


hasWinningTile : Board -> Bool
hasWinningTile board =
    List.any (\t -> t.value == 2048) board


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
            Random.map2
                (\cell n ->
                    let
                        ( row, col ) =
                            takeCell cell
                    in
                        TileData row
                            col
                            (if n == 7 then
                                4
                             else
                                2
                            )
                )
                (Random.int 1 (List.length empty))
                (Random.int 1 14)
    in
        Random.generate AddTile generator


addRandomTileOnInitCmd : Model -> Cmd Msg
addRandomTileOnInitCmd model =
    if isBoardReady model.board then
        Cmd.none
    else
        addRandomTileCmd model


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
            | layout = Layout gameWidth gameLeft cellWidth borderWidth
            , board =
                model.board
                    |> List.map (resizeTile cellWidth model.time)
        }


isRunning : Model -> Bool
isRunning {board, popup, nextMsg} =
    isBoardReady board || nextMsg /= NoOp


isAnimating : Model -> Bool
isAnimating {popup, time, board} =
    let
        isBoardAnimationRunning_ tiles =
            case tiles of
                [] ->
                    False

                tile :: tail ->
                    not (isDone time tile.row)
                        || not (isDone time tile.col)
                        || not (isDone time tile.size)
                        || isBoardAnimationRunning_ tail
    in
        popup ==Nothing && isBoardAnimationRunning_ board
