module P4 exposing (view)

import Debug
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , column
        , el
        , none
        , row
        , spacing
        , text
        )
import Parser exposing (..)
import Set exposing (Set)


type alias Model a =
    { a
        | text : Maybe String
        , state : Dict String Bool
    }


dash =
    symbol "-"


span : Parser ( Int, Int )
span =
    succeed Tuple.pair
        |= lazy (\_ -> int)
        |. dash
        |= lazy (\_ -> int)


nonDecending : Int -> Int -> Int -> Int -> List Int
nonDecending num last current length =
    if current == length - 1 then
        List.range last 9
            |> List.map (\d -> num * 10 + d)

    else
        List.range last 9
            |> List.map (\d -> ( d, num * 10 + d ))
            |> List.concatMap (\( d1, num1 ) -> nonDecending num1 d1 (current + 1) length)


hasDouble : Int -> Bool
hasDouble num =
    let
        pred : List Int -> Bool
        pred xs =
            case xs of
                y1 :: y2 :: rest ->
                    y1 == y2 || pred (y2 :: rest)

                _ ->
                    False
    in
    num
        |> intToList
        >> pred


log10 : Int -> Int
log10 n =
    logBase 10 (toFloat n)
        |> floor


intToList : Int -> List Int
intToList n =
    let
        diggits =
            log10 n

        p =
            10 ^ diggits
    in
    if diggits == 0 then
        [ n ]

    else
        n // p :: intToList (modBy p n)


part1 : Model a -> Element msg
part1 model =
    column [ spacing 20 ]
        [ text "Problem dictates 6 diggit numbers."
        , model.text
            |> Maybe.withDefault question
            >> run span
            >> Result.map
                (\( start, stop ) ->
                    List.range 1 9
                        |> List.concatMap (\x -> nonDecending x x 1 6)
                        >> List.filter (\x -> x > start && x < stop && hasDouble x)
                        >> List.length
                        >> String.fromInt
                        >> (++) "Solution := "
                        >> text
                )
            |> Result.withDefault (text "Invalid Input")
        ]


hasSpecialDouble : Int -> Bool
hasSpecialDouble n =
    let
        helper : List Int -> Bool
        helper xs =
            case xs of
                -- 2 same between 2 not same.
                x1 :: x2 :: x3 :: x4 :: rest ->
                    x1 /= x2 && x2 == x3 && x3 /= x4 || helper (x2 :: x3 :: x4 :: rest)

                _ ->
                    False
    in
    n
        |> intToList
        >> (\x -> 0 :: x ++ [ 0 ])
        >> helper


part2 : Model a -> Element msg
part2 model =
    column []
        [ model.text
            |> Maybe.withDefault question
            >> run span
            >> Result.map
                (\( start, stop ) ->
                    List.range 1 9
                        |> List.concatMap (\x -> nonDecending x x 1 6)
                        >> List.filter (\x -> x > start && x < stop && hasSpecialDouble x)
                        >> List.length
                        >> String.fromInt
                        >> (++) "Solution := "
                        >> text
                )
            |> Result.withDefault (text "Invalid Input")
        ]


view :
    Model a
    -> ((Model a -> Model a) -> msg)
    -> ( Element msg, Element msg )
view model update =
    ( part1 model, part2 model )


question =
    "147981-691423"
