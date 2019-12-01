module P1 exposing (view)

import Element
    exposing
        ( Element
        , el
        , none
        , text
        )


type alias Model a =
    { a
        | text : Maybe String
    }


fuleFormula : Int -> Int
fuleFormula fule =
    max (fule // 3 - 2) 0


answer : (Int -> Int) -> String -> String
answer fn ans =
    ans
        |> String.lines
        |> List.filterMap String.toInt
        |> (List.map fn >> List.sum)
        |> String.fromInt


findTotalFule : Int -> Int
findTotalFule fule =
    if fule <= 6 then
        0

    else
        fuleFormula fule
            |> (\s -> s + findTotalFule s)


answerView : Model a -> (Int -> Int) -> Element msg
answerView model fn =
    model.text
        |> Maybe.map
            (answer fn
                >> text
                >> el []
            )
        |> Maybe.withDefault none


view : Model a -> ( Element msg, Element msg )
view model =
    ( answerView model fuleFormula, answerView model findTotalFule )
