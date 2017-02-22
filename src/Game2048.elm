module Game2048 exposing (main)

import Html exposing (Html)
import Keyboard
import Debug
import AnimationFrame
import Game2048.Model as M
    exposing
        ( Model
        , Msg(..)
        , Direction(..)
        , TileData
        , Size
        )
import Game2048.View exposing (view)
import Game2048.Ports exposing (size, save)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ( time, initialBoard ) ->
            let
                nextModel =
                    M.addTiles initialBoard ({ model | time = time })
            in
                nextModel ! [ M.addRandomTileOnInitCmd nextModel ]

        Resize size ->
            M.resize size model ! []

        AddTile tileData ->
            let
                nextModel =
                    M.addTile model tileData
            in
                nextModel
                    ! [ M.addRandomTileOnInitCmd nextModel
                      , save (M.saveData nextModel)
                      ]

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
            init 0 model.best (\m -> { m | layout = model.layout }) []

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
            [ running, size Resize ]


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


init : Int -> Int -> (Model -> Model) -> List TileData -> ( Model, Cmd Msg )
init score best resizeFn initialBoard =
    let
        model =
            M.init score best |> resizeFn
    in
        model ! [ M.initCmd model initialBoard ]


type alias Flags =
    { score : Int
    , best : Int
    , size : Size
    , initialBoard : List TileData
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init =
            (\{ score, best, size, initialBoard } ->
                init score best (M.resize size) initialBoard
            )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
