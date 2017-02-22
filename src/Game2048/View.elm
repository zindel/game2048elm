module Game2048.View exposing (..)

import Game2048.Model
    exposing
        ( Model
        , Msg(..)
        , Tile
        , Layout
        , ScoreUpdate
        , Popup
        , PopupType(..)
        , boardSize
        )
import Html exposing (Html)
import Html.Attributes exposing (style, src)
import Html.Events  exposing (onClick)
import Time exposing (Time)
import Animation exposing (animate)
import Color exposing (Color, rgba, rgb)
import Game2048.Util exposing (cells, colorToCSS)


toTopLeft : Layout -> Float -> Float -> Float -> ( Float, Float )
toTopLeft { borderWidth, cellWidth } row col size =
    let
        center =
            (cellWidth - size) / 2.0
    in
        ( center + borderWidth * row + (row - 1) * cellWidth
        , center + borderWidth * col + (col - 1) * cellWidth
        )


viewBox : Float -> Float -> Float -> Color -> Color -> String -> Html Msg
viewBox left top size color textColor text =
    let
        floatLen =
            toFloat (String.length text)

        fontSize =
            size / (floatLen / 2 + 1 |> max 2)
    in
        Html.div
            [ style
                [ ( "position", "absolute" )
                , ( "left", toString left ++ "px" )
                , ( "top", toString top ++ "px" )
                , ( "width", toString size ++ "px" )
                , ( "height", toString size ++ "px" )
                , ( "line-height", toString size ++ "px" )
                , ( "backgroundColor", colorToCSS color )
                , ( "color", colorToCSS textColor )
                , ( "fontWeight", "bold" )
                , ( "fontSize", toString fontSize ++ "px" )
                , ( "textAlign", "center" )
                , ( "verticalAlign", "middle" )
                , ( "borderRadius", "3px" )
                ]
            ]
            [ Html.text text ]


viewCell : Layout -> ( Int, Int ) -> Html Msg
viewCell layout ( row, col ) =
    let
        ( top, left ) =
            toTopLeft layout (toFloat row) (toFloat col) layout.cellWidth
    in
        viewBox left top layout.cellWidth (rgba 238 228 218 0.35) Color.black ""


viewCells : Layout -> List (Html Msg)
viewCells layout =
    cells boardSize |> List.map (viewCell layout)


tileBackgroundColor : Int -> Color
tileBackgroundColor value =
    let
        color =
            [ rgb 238 228 218
            , rgb 237 224 200
            , rgb 242 177 121
            , rgb 245 149 99
            , rgb 246 124 95
            , rgb 246 94 59
            , rgb 237 207 114
            , rgb 237 204 97
            , rgb 237 200 80
            , rgb 237 197 63
            , rgb 237 194 46
            ]

        drop =
            logBase 2 (toFloat value) - 1 |> floor
    in
        color
            |> List.drop drop
            |> List.head
            |> Maybe.withDefault Color.black


tileTextColor : Int -> Color
tileTextColor value =
    if value < 8 then
        rgb 119 110 101
    else
        rgb 249 246 242


viewTile : Layout -> Time -> Tile -> Html Msg
viewTile layout time tile =
    let
        getValue =
            animate time

        ( top, left ) =
            toTopLeft layout
                (getValue tile.row)
                (getValue tile.col)
                (getValue tile.size)
    in
        viewBox left top (getValue tile.size) (tileBackgroundColor tile.value) (tileTextColor tile.value) (toString tile.value)


viewPopup : Time -> Layout -> Popup -> Html Msg
viewPopup time { cellWidth, width } { popupType, opacity } =
    let
        buttonStyle =
            [ ( "marginLeft", "5px" ) ]

        ( message, buttons ) =
            case popupType of
                Victory ->
                    ( "You won!"
                    , [ button cellWidth buttonStyle "Continue Playing" Continue
                      , button cellWidth buttonStyle "New Game" NewGame
                      ]
                    )

                GameOver ->
                    ( "Game Over."
                    , [ button cellWidth buttonStyle "Try Again" NewGame
                      ]
                    )

        color =
            animate time opacity |> rgba 255 255 255 |> colorToCSS

        fontSize =
            cellWidth / 2

        messageDiv =
            Html.div
                [ style
                    [ ( "fontSize", toString fontSize ++ "px" )
                    , ( "fontWeight", "bold" )
                    , ( "textAlign", "center" )
                    , ( "marginTop", toString cellWidth ++ "px" )
                    , ( "color", "#776e65" )
                    ]
                ]
                [ Html.text message ]

        buttonDiv =
            Html.div [ style [ ( "textAlign", "center" ) ] ]
    in
        Html.div
            [ style
                [ ( "position", "absolute" )
                , ( "backgroundColor", color )
                , ( "width", toString width ++ "px" )
                , ( "height", toString width ++ "px" )
                ]
            ]
            [ messageDiv, buttons |> buttonDiv ]


viewBoard : Model -> Html Msg
viewBoard { popup, layout, time, board } =
    let
        viewTiles =
            List.map (viewTile layout time) board

        viewPopup_ =
            case popup of
                Nothing ->
                    []

                Just popup_ ->
                    [ viewPopup time layout popup_ ]
    in
        viewCells layout
            ++ viewTiles
            ++ viewPopup_
            |> Html.div
                [ style
                    [ ( "position", "relative" )
                    , ( "width", toString layout.width ++ "px" )
                    , ( "height", toString layout.width ++ "px" )
                    , ( "borderRadius", "3px" )
                    , ( "backgroundColor", "#bbada0" )
                    ]
                ]


viewLogo : Layout -> Html Msg
viewLogo { cellWidth } =
    let
        fontSize =
            cellWidth / 2

        imgWidth =
            fontSize / 1.5
    in
        Html.div
            [ style
                [ ( "fontWeight", "bold" )
                , ( "fontSize", toString fontSize ++ "px" )
                , ( "color", "#776e65" )
                ]
            ]
            [ Html.img
                [ style
                    [ ( "width", toString imgWidth ++ "px" )
                    , ( "marginRight", "5px" )
                    ]
                , src "http://elm-lang.org/assets/logo.svg"
                ]
                []
            , Html.text "2048"
            ]


viewTopText : Layout -> Html Msg
viewTopText { cellWidth } =
    let
        fontSize =
            cellWidth / 6
    in
        Html.div
            [ style
                [ ( "fontSize", toString fontSize ++ "px" )
                , ( "color", "#776e65" )
                ]
            ]
            [ Html.text "Join numbers to get the "
            , Html.b [] [ Html.text "2048" ]
            , Html.text " tile!"
            , Html.br [] []
            , Html.text "Written in Elm just for fun."
            ]


viewScoreBox : Layout -> String -> Int -> List (Html Msg) -> Html Msg
viewScoreBox { cellWidth } title value scoreUpdates =
    let
        fontSize =
            cellWidth / 5

        titleDiv =
            Html.div
                [ style [ ( "fontSize", "60%" ), ( "color", "#eee4da" ) ] ]
                [ Html.text title ]

        numberDiv =
            Html.div [] [ Html.text (toString value) ]

        runningDiv =
            Html.div
                [ style
                    [ ( "position", "absolute" )
                    , ( "display", "inline-block" )
                    , ( "width", "auto" )
                    , ( "height", "auto" )
                    , ( "top", "-20px" )
                    , ( "left", "20px" )
                    ]
                ]
                [ Html.text "See" ]
    in
        Html.div
            [ style
                [ ( "position", "relative" )
                , ( "display", "inline-block" )
                , ( "height", toString (cellWidth * 0.43) ++ "px" )
                , ( "width", toString (cellWidth * 0.85) ++ "px" )
                , ( "marginLeft", "6px" )
                , ( "padding", "3px" )
                , ( "fontSize", toString fontSize ++ "px" )
                , ( "lineHeight", toString fontSize ++ "px" )
                , ( "fontWeight", "bold" )
                , ( "textAlign", "center" )
                , ( "backgroundColor", "#bbada0" )
                , ( "color", "white" )
                , ( "borderRadius", "3px" )
                ]
            ]
            ([ titleDiv, numberDiv ] ++ scoreUpdates)


viewScoreUpdate : Time -> ScoreUpdate -> Html Msg
viewScoreUpdate time { value, index, animation } =
    let
        left =
            10 + index * 10

        top =
            (animate time animation) * 20
    in
        Html.div
            [ style
                [ ( "position", "absolute" )
                , ( "display", "inline-block" )
                , ( "width", "auto" )
                , ( "height", "auto" )
                , ( "color", colorToCSS (rgba 143 122 102 0.7) )
                , ( "top", toString top ++ "px" )
                , ( "left", toString left ++ "px" )
                ]
            ]
            [ Html.text ("+" ++ toString value) ]


viewScore : Model -> Html Msg
viewScore model =
    let
        scoreUpdates =
            model.scoreUpdates
                |> List.map (viewScoreUpdate model.time)
    in
        Html.div
            [ style
                [ ( "position", "absolute" )
                , ( "top", "4px" )
                , ( "right", "0px" )
                , ( "height", "auto" )
                , ( "width", "auto" )
                ]
            ]
            [ viewScoreBox model.layout "SCORE" model.score scoreUpdates
            , viewScoreBox model.layout "BEST" model.best []
            ]


button : Float -> List ( String, String ) -> String -> Msg -> Html Msg
button cellWidth styles text msg =
    let
        fontSize =
            cellWidth / 5
    in
        Html.button
            [ style
                ([ ( "padding", "11px" )
                 , ( "color", "#f9f6f2" )
                 , ( "backgroundColor", "#8f7a66" )
                 , ( "borderRadius", "3px" )
                 , ( "fontWeight", "bold" )
                 , ( "border", "none" )
                 , ( "cursor", "pointer" )
                 , ( "fontSize", toString fontSize ++ "px" )
                 , ( "lineHeight", toString fontSize ++ "px" )
                 ]
                    ++ styles
                )
            , onClick msg    
            ]
            [ Html.text text ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.div
        [ style
            [ ( "position", "relative" )
            , ( "width", "auto" )
            , ( "minHeight", "55px" )
            ]
        ]
        [ viewLogo model.layout
        , viewScore model
        ]


viewHeader2 : Model -> Html Msg
viewHeader2 model =
    Html.div [ style [ ( "minHeight", "50px" ) ] ]
        [ button
            model.layout.cellWidth
            [ ( "float", "right" ) ]
            "New Game"
            NewGame
        , viewTopText model.layout
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ style
            [ ( "position", "relative" )
            -- , ( "left", toString model.layout.left ++ "px" )
            -- , ( "top", "0px" )
            , ( "paddingTop", "20px" )
            , ( "marginLeft", toString model.layout.padding ++ "px" )
            , ( "width", toString model.layout.width ++ "px" )
            , ( "height", "auto" )
            ]
        ]
        [ viewHeader model
        , viewHeader2 model
        , viewBoard model
        ]
