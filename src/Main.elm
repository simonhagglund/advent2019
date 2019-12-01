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
    | Update (Model -> Model)


type View
    = P1
    | P2


type alias Model =
    { text : Maybe String
    , view : View
    }


type alias Renderer =
    Model -> ( Element Msg, Element Msg )


views : List ( View, Renderer )
views =
    [ ( P1, P1.view )
    , ( P2, always ( text "P2", text "P2" ) )
    ]


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

        Update fn ->
            fn model


view : Model -> Html Msg
view model =
    getRenderer model.view views
        |> problemView model


getRenderer : View -> List ( View, Renderer ) -> Renderer
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
                |> (\e _ -> ( e, e ))


viewProblems : Model -> Element Msg
viewProblems model =
    let
        b : View -> Element Msg
        b v =
            Input.button
                [ Border.width 1
                , Border.color borderColor
                , Border.solid
                , Border.rounded 10
                , padding 4
                ]
                { onPress =
                    Update (\m -> { m | view = v })
                        |> Just
                , label = Debug.toString v |> text
                }
    in
    List.map (Tuple.first >> b) views
        |> row
            [ spacing 3
            , padding 3
            ]


problemView : Model -> Renderer -> Html Msg
problemView model renderer =
    let
        ( part1, part2 ) =
            renderer model

        answers =
            [ ( "Part 1", part1 ), ( "Part 2", part2 ) ]
    in
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
            , answers
                |> List.concatMap
                    (\( title, part ) ->
                        [ text title
                        , part
                            |> el
                                [ width fill
                                , Border.width 1
                                , 380 |> px >> height
                                , Border.color borderColor
                                , Border.solid
                                , moveDown 2
                                , padding 10
                                ]
                        ]
                    )
                |> column
                    [ width fill
                    , spacing 10
                    , moveUp 4
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
        { onChange =
            \str ->
                Update
                    (\m ->
                        { m
                            | text =
                                if str /= "" then
                                    Just str

                                else
                                    Nothing
                        }
                    )
        , text = Maybe.withDefault "" model.text
        , placeholder = Nothing
        , label =
            text "Probem data"
                |> Input.labelAbove [ moveUp 7 ]
        , spellcheck = False
        }
