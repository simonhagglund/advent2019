module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import P1
import P2


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
    , state : Dict String Bool
    }


type alias Renderer =
    Model
    -> ((Model -> Model) -> Msg)
    -> ( Element Msg, Element Msg )


views : List ( View, Renderer )
views =
    [ ( P1, P1.view )
    , ( P2, P2.view )
    ]


defaultView =
    P2


defaultText =
    "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0"


init =
    { text = Just defaultText
    , view = defaultView
    , state = Dict.empty
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
                |> (\e _ _ -> ( e, e ))


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
            renderer model Update

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
                |> el
                    [ width fill
                    , HA.style "word-break" "break-all"
                        |> htmlAttribute
                    ]
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
            text "Problem data"
                |> Input.labelAbove [ moveUp 7 ]
        , spellcheck = False
        }
