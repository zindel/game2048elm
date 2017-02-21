module Game2048 exposing (main)

import Html exposing (Html)
import Keyboard
import Debug
import AnimationFrame
import Animation exposing (isDone)
import Time exposing (Time)
import Window
import Task

import Game2048.Model as M exposing (Model, Msg(..), Direction(..))
import Game2048.View exposing (view)

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        initModel initialBoard model =
            case initialBoard of
                [] ->
                    model

                tileData :: tail ->
                    M.addTile model tileData |> initModel tail
    in
        case msg of
            Init time ->
                let
                    nextModel =
                        initModel model.initialBoard ({ model | time = time })
                in
                    nextModel ! [ M.addRandomTileOnInitCmd nextModel ]

            Resize size ->
                M.resize size model ! []

            AddTile tileData ->
                let
                    nextModel =
                        M.addTile model tileData
                in
                    { nextModel | nextMsg = NoOp }
                        ! [ M.addRandomTileOnInitCmd nextModel ]

            Move direction ->
                M.move direction model ! []

            Collapse ->
                M.collapse model ! []

            AddRandomTile ->
                model ! [ M.addRandomTileCmd model ]

            Tick time ->
                let
                    nextModel =
                        { model | time = time }
                in
                    if isAnimationRunning nextModel then
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
            if M.isRunning model then
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
    Html.program
        { init =
            M.init []
                ! [ Task.perform Resize Window.size
                  , Task.perform Init Time.now
                  ]
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
