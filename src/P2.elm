module P2 exposing (view)

import Array exposing (Array)
import Debug
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , htmlAttribute
        , none
        , padding
        , rgba255
        , row
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Html.Attributes as HA
import Parser exposing (..)


borderColor =
    rgba255 186 189 182 1.0


type alias Program =
    { instruction : String
    , memory : Array Int
    , pc : Int
    }


type alias Model a =
    { a
        | text : Maybe String
        , state : Dict String Bool
    }


type alias LA =
    Int


type alias SA =
    Int


type OpCode
    = Op99
    | Op2 LA LA SA
    | Op1 LA LA SA


comma =
    symbol ","


opcode1 : Parser OpCode
opcode1 =
    succeed (always Op1)
        |= map (\_ -> int) (keyword "1")
        |. comma
        |= lazy (\_ -> int)
        |. comma
        |= lazy (\_ -> int)
        |. comma
        |= lazy (\_ -> int)


opcode2 : Parser OpCode
opcode2 =
    succeed (always Op2)
        |= map (\_ -> int) (keyword "2")
        |. comma
        |= lazy (\_ -> int)
        |. comma
        |= lazy (\_ -> int)
        |. comma
        |= lazy (\_ -> int)


opcode99 : Parser OpCode
opcode99 =
    map (always Op99) (keyword "99")


opCode : Parser OpCode
opCode =
    oneOf
        [ opcode1
        , opcode2
        , opcode99
        ]


incPc : Program -> Program
incPc program =
    { program
        | pc = program.pc + 1
        , instruction =
            program.memory
                |> Array.toList
                |> List.drop (4 * program.pc)
                |> List.take 4
                |> List.map String.fromInt
                |> String.join ","
    }


execute : Program -> Array Int
execute program =
    let
        store sa imidiate =
            program.memory
                |> Array.set sa imidiate
                >> (\mem -> { program | memory = mem })
    in
    -- Fetch
    case run opCode program.instruction of
        -- Decode
        Ok op ->
            -- Execute
            case op of
                Op1 la1 la2 sa ->
                    case ( Array.get la1 program.memory, Array.get la2 program.memory ) of
                        ( Just i1, Just i2 ) ->
                            -- Store
                            (i1 + i2)
                                |> store sa
                                >> incPc
                                >> execute

                        _ ->
                            program.memory

                Op2 la1 la2 sa ->
                    case ( Array.get la1 program.memory, Array.get la2 program.memory ) of
                        -- Store
                        ( Just i1, Just i2 ) ->
                            (i1 * i2)
                                |> store sa
                                >> incPc
                                >> execute

                        _ ->
                            program.memory

                Op99 ->
                    program.memory

        Err err ->
            let
                _ =
                    Debug.log "err" err
            in
            program.memory


run_program : String -> Array Int
run_program raw =
    let
        program =
            { pc = 0
            , instruction = ""
            , memory =
                raw
                    |> String.split ","
                    >> List.filterMap String.toInt
                    >> Array.fromList
            }
    in
    program
        |> incPc
        |> execute


part1 : Model a -> Element msg
part1 model =
    let
        tests =
            [ "1,0,0,0,99"
            , "2,3,0,3,99"
            , "2,4,4,5,99,0"
            , "1,1,1,4,99,5,6,0,99"
            ]
    in
    column
        [ width fill ]
        [ text "Tests"
        , tests
            |> List.map
                (\p ->
                    p
                        |> run_program
                        >> Array.toList
                        >> List.map String.fromInt
                        >> String.join ","
                        >> (\mem -> row [ spacing 10 ] [ text <| p ++ "=" ++ mem ])
                )
            |> column []
        , text "Solution"
        , model.text
            |> Maybe.map (run_program >> Array.toList >> List.head >> Maybe.map String.fromInt)
            >> lift4A
            >> Maybe.withDefault ""
            >> text
            >> el
                [ HA.style "word-break" "break-all"
                    |> htmlAttribute
                , width fill
                ]
        ]


lift4A : Maybe (Maybe a) -> Maybe a
lift4A m =
    case m of
        Just (Just a) ->
            Just a

        _ ->
            Nothing


part2 : Model a -> Element msg
part2 model =
    let
        ns =
            List.range 0 100
                |> List.concatMap
                    (\x1 ->
                        List.range 0 100
                            |> List.map (\x2 -> ( x1, x2 ))
                    )

        first =
            program
                |> (List.head >> Maybe.withDefault 0)

        program =
            model.text
                |> Maybe.map (String.split "," >> List.filterMap String.toInt)
                |> Maybe.withDefault []

        ps =
            ns
                |> List.map
                    (\( x1, x2 ) ->
                        first
                            :: x1
                            :: x2
                            :: List.drop 3 program
                            |> List.map String.fromInt
                            >> String.join ","
                    )

        results =
            ps
                |> List.filterMap
                    (run_program
                        >> Array.toList
                        >> (\xs ->
                                case List.head xs of
                                    Just x ->
                                        if x == 19690720 then
                                            Just xs

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                           )
                    )
                |> List.map (List.take 3 >> List.map String.fromInt >> String.join ",")
                >> String.join ","
    in
    column
        [ width fill ]
        [ results |> text ]


view :
    Model a
    -> ((Model a -> Model a) -> msg)
    -> ( Element msg, Element msg )
view model update =
    let
        open =
            model.state
                |> Dict.get "p2_open"
                |> Maybe.withDefault False
    in
    if open then
        ( part1 model, part2 model )

    else
        ( text "Run IntCode"
            |> el
                [ update
                    (\m ->
                        { m | state = m.state |> Dict.insert "p2_open" True }
                    )
                    |> Events.onClick
                , Border.width 1
                , Border.color borderColor
                , Border.solid
                , Border.rounded 10
                , padding 4
                ]
        , none
        )
