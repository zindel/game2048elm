module Game2048 exposing (main)

import Game2048.Model  exposing (Model, Msg(..), Direction(..), boardSize, init, isBoardReady, collapseBoard, moveBoard, addTile, resizeTile, TileData, emptyCells)

import Game2048.View exposing (view, borderWidth)

import Html exposing (Html)
import Keyboard
import List
import Debug
import AnimationFrame
import Animation exposing (isDone)
import Time exposing (Time)
import Window
import Task
import Random


-- MODEL



-- UPDATE


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
            Init time ->
                let
                    nextModel =
                        initModel model.initialBoard ({ model | time = time })
                in
                    nextModel ! [ addRandomTileOnInitCmd nextModel ]

            Resize { width, height } ->
                let
                    gameWidth =
                        toFloat height
                            * 0.7
                            |> min (toFloat width * 0.6)
                            |> max 300.0

                    gameLeft =
                        toFloat width / 2.0 - gameWidth / 2 |> max 0

                    cellWidth =
                        (gameWidth - borderWidth * (boardSize + 1)) / boardSize
                in
                    { model
                        | width = gameWidth
                        , left = gameLeft
                        , cellWidth = cellWidth
                        , board =
                            model.board
                                |> List.map (resizeTile cellWidth model.time)
                    }
                        ! []

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

        running =
            if isBoardReady model.board || model.nextMsg /= NoOp then
                Sub.batch
                    [ if model.nextMsg == NoOp then
                        Sub.map direction (Keyboard.downs identity)
                      else
                        Sub.none
                    , AnimationFrame.times Tick
                    ]
            else
                Sub.none
    in
        Sub.batch
            [ running, Window.resizes Resize ]



-- UTILS


range : List Int
range =
    List.range 1 boardSize





-- MAIN


main : Program Never Model Msg
main =
    let
        initialModel =
            init []
    in
        Html.program
            { init =
                initialModel
                    ! [ Task.perform Resize Window.size
                      , Task.perform Init Time.now
                      ]
            , subscriptions = subscriptions
            , update = update
            , view = view
            }
