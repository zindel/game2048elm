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
            ]
      , time = 0
      }
    , Cmd.none
    )



-- UPDATE


updateBoard : Time -> Int -> Board -> Board
updateBoard time key board =
    let
        fn =
            case key of
                37 ->
                    \row col ->
                        ( row
                        , if col == 1 then
                            4
                          else
                            1
                        )

                _ ->
                    \row col -> (row, col)

        move s e =
            if s == e then
                static s
            else
                animation time |> from s |> to e |> duration (0.3 * second)

        updateTile : Tile -> Tile
        updateTile tile =
            let
                startRow =
                    getTo tile.row

                startCol =
                    getTo tile.col

                ( endRow, endCol ) =
                    fn startRow startCol
            in
                { tile
                    | row = move startRow endRow
                    , col = move startCol endCol
                }
    in
        Debug.log "tiles" List.map updateTile board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArrowKey code ->
            ( { model
                | board = updateBoard model.time code model.board
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
                    let
                        moving =
                            not (isDone time tile.row)
                            || not (isDone time tile.col)
                    in
                        moving || isMoving_ time tail
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
            toTopLeft (animate time tile.row, animate time tile.col)
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



-- if not model.isMoving then
--     Sub.batch
--         [ Keyboard.downs ArrowKey
--         ]
-- else
--     Sub.batch
--         [ AnimationFrame.times Tick
--         ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
