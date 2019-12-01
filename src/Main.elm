module Main exposing (main)

import Browser
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)


blue =
    Element.rgb255 238 238 238


purple =
    Element.rgb255 128 0 128


type Msg
    = Noop
    | UpdateInputText String
    | SubmitInput


type alias Model =
    { text : Maybe String
    , fule : Maybe Int
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

        SubmitInput ->
            model


fuleFormula : Int -> Int
fuleFormula fule =
    fule // 3 - 2


answer : (Int -> Int) -> String -> String
answer fn ans =
    ans
        |> String.lines
        |> List.filterMap String.toInt
        |> List.foldl (\a b -> b + fn a) 0
        |> String.fromInt


findTotalFule : Int -> Int
findTotalFule fule =
    if fule <= 6 then
        0

    else
        fuleFormula fule
            |> (\s -> s + findTotalFule s)


answerView : Model -> ( String, Int -> Int ) -> List (Element Msg)
answerView model ( title, fn ) =
    [ text title
    , model.text
        |> Maybe.map
            (answer fn
                >> text
                >> el [ moveRight 20 ]
            )
        |> Maybe.withDefault none
    ]


view model =
    let
        answers =
            [ ( "Part 1", fuleFormula ), ( "Part 2", findTotalFule ) ]
    in
    row
        [ width fill ]
        [ column [ width fill ]
            [ inputView model
                |> el [ width fill ]
            ]
        , answers
            |> List.concatMap (answerView model)
            |> column [ width fill, alignTop, width fill, paddingXY 10 40 ]
        ]
        |> layout []


inputView : Model -> Element Msg
inputView model =
    let
        currentText =
            Maybe.withDefault "" model.text
    in
    Input.multiline
        [ width fill
        , 800 |> px >> height
        ]
        { onChange = UpdateInputText
        , text = currentText
        , placeholder = Nothing
        , label =
            text "Input Problem"
                |> Input.labelAbove []
        , spellcheck = False
        }


init =
    { text = Nothing
    , fule = Nothing
    }
