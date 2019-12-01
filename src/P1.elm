module P1 exposing (..)

import Element exposing (..)


type alias Model a =
    { a
        | text : Maybe String
    }


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


answerView : Model a -> ( String, Int -> Int ) -> List (Element msg)
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


view : Model a -> Element msg
view model =
    let
        answers =
            [ ( "Part 1", fuleFormula ), ( "Part 2", findTotalFule ) ]
    in
    answers
        |> List.concatMap (answerView model)
        |> column [ width fill, alignTop, width fill, paddingXY 10 40 ]
