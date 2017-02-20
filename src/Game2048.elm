module Game2048 exposing (main)

import Game2048.Model exposing (Model, Msg(..), Direction(..), boardSize, init, isBoardReady, collapse, move, addTile, resize, isRunning, addRandomTileCmd)
import Game2048.View exposing (view)
import Html exposing (Html)
import Keyboard
import Debug
import AnimationFrame
import Animation exposing (isDone)
import Time exposing (Time)
import Window
import Task


-- MODEL
-- UPDATE


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

            Resize size ->
                resize size model ! []

            AddTile tileData ->
                let
                    nextModel =
                        addTile model tileData
                in
                    { nextModel | nextMsg = NoOp }
                        ! [ addRandomTileOnInitCmd nextModel ]

            Move direction ->
                move direction model ! []

            Collapse ->
                collapse model ! []

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
            if isRunning model then
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
