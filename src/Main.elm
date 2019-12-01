module Main exposing (main)

import Browser
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import P1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


borderColor =
    rgba255 186 189 182 1.0


type Msg
    = Noop
    | UpdateInputText String
    | ChangeProblem Float
    | Update (Model -> Model)


type View
    = P1
    | P2


type alias Model =
    { text : Maybe String
    , view : View
    }


views : List ( View, Model -> Element Msg )
views =
    [ ( P1, P1.view ), ( P2, always (text "P2") ) ]


defaultView =
    P1


init =
    { text = Nothing
    , view = defaultView
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        UpdateInputText str ->
            { model
                | text = Just str
            }

        ChangeProblem float ->
            model

        Update fn ->
            fn model


view : Model -> Html Msg
view model =
    getRenderer model.view views
        |> problemView model


getRenderer : View -> List ( View, Model -> Element Msg ) -> (Model -> Element Msg)
getRenderer v xs =
    case xs of
        ( x, r ) :: ys ->
            if v == x then
                r

            else
                getRenderer v ys

        [] ->
            text "Renderer not found"
                |> el [ alignTop, width fill ]
                |> always


viewProblems model =
    let
        b : ( View, Model -> Element Msg ) -> Element Msg
        b ( v, _ ) =
            Input.button
                [ Border.width 1
                , Border.color borderColor
                , Border.solid
                ]
                { onPress =
                    Update (\m -> { m | view = v })
                        |> Just
                , label = Debug.toString v |> text
                }
    in
    List.map b views
        |> row
            [ spacing 3
            , padding 3
            ]


problemView : Model -> (Model -> Element Msg) -> Html Msg
problemView model renderer =
    column
        [ width fill
        , paddingXY 300 30
        ]
        [ row
            [ width fill
            , spacing 20
            ]
            [ inputView model
                |> el [ width fill ]
            , column
                [ width fill
                ]
                [ text "Solution"
                    |> el [ moveUp 5 ]
                , renderer model
                    |> el
                        [ width fill
                        , Border.width 1
                        , 800 |> px >> height
                        , Border.color borderColor
                        , Border.solid
                        , moveDown 2
                        ]
                ]
            ]
        , viewProblems model
        ]
        |> layout []


inputView : Model -> Element Msg
inputView model =
    Input.multiline
        [ width fill
        , 800 |> px >> height
        ]
        { onChange = UpdateInputText
        , text = Maybe.withDefault "" model.text
        , placeholder = Nothing
        , label =
            text "Input Problem"
                |> Input.labelAbove [ moveUp 7 ]
        , spellcheck = False
        }
