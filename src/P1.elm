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


fuelFormula : Int -> Int
fuelFormula fuel =
    max (fuel // 3 - 2) 0


answer : (Int -> Int) -> String -> String
answer fn ans =
    ans
        |> String.lines
        |> List.filterMap String.toInt
        |> (List.map fn >> List.sum)
        |> String.fromInt


totalFuel : Int -> Int
totalFuel fuel =
    if fuel <= 6 then
        0

    else
        fuelFormula fuel
            |> (\s -> s + totalFuel s)


answerView : Model a -> (Int -> Int) -> Element msg
answerView model fn =
    model.text
        |> Maybe.map
            (answer fn
                >> text
                >> el []
            )
        |> Maybe.withDefault none


view :
    Model a
    -> ((Model a -> Model a) -> msg)
    -> ( Element msg, Element msg )
view model _ =
    ( answerView model fuelFormula, answerView model totalFuel )
