module Game2048 exposing (main)

import Html exposing (Html)
import Keyboard
import Debug
import AnimationFrame
import Animation exposing (isDone)
import Time exposing (Time)
import Window
import Task
import Game2048.Model as M exposing (Model, Msg(..), Direction(..), TileData)
import Game2048.View exposing (view)


-- UPDATE


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ Task.perform Resize Window.size
        , Task.perform Init Time.now
        ]


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
                    nextModel ! [ M.addRandomTileOnInitCmd nextModel ]

            Move direction ->
                if M.canMoveTo direction model then
                    M.move direction model ! []
                else
                    model ! []

            Collapse ->
                M.collapse model ! []

            AddRandomTile ->
                model ! [ M.addRandomTileCmd model ]

            ShowPopup popupType ->
                M.createPopup (Debug.log "PT" popupType) model ! []

            NewGame ->
                M.init model.best [] ! [ initCmd ]

            Continue ->
                { model | freePlay = True, popup = Nothing }
                    ! [ M.addRandomTileCmd model ]

            Tick time ->
                let
                    nextModel =
                        M.clearScoreUpdate { model | time = time }
                in
                    if M.isAnimating nextModel then
                        nextModel ! []
                    else
                        update model.nextMsg nextModel

            NoOp ->
                model ! []



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


beforeWin =
    [ TileData 1 1 1024
    , TileData 1 2 1024
    , TileData 1 3 512
    , TileData 1 4 256
    ]


beforeFail =
    [ TileData 1 1 1024
    , TileData 1 2 512
    , TileData 1 3 256
    , TileData 1 4 128
    , TileData 2 1 64
    , TileData 2 2 32
    , TileData 2 3 16
    , TileData 2 4 8
    , TileData 3 1 1024
    , TileData 3 2 512
    , TileData 3 3 256
    , TileData 3 4 64
    , TileData 4 1 32
    , TileData 4 2 64
    , TileData 4 3 32
    ]


main : Program Never Model Msg
main =
    Html.program
        { init =
            M.init 0
                []
                -- beforeFail
                ! [ initCmd ]
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
