module Tests exposing (..)

import Decoder
import Examples.Custom as Custom
import Examples.Daily as Daily
import Examples.Monthly as Monthly
import Examples.Types exposing (Example)
import Examples.Weekly as Weekly
import Examples.Yearly as Yearly
import Expect
import Generator
import Test exposing (Test, describe)
import Time exposing (Posix)


daily : Test
daily =
    testExamples "Daily"
        [ Daily.example1
        , Daily.example2
        , Daily.example3
        , Daily.example4
        , Daily.example5
        ]


weekly : Test
weekly =
    testExamples "Weekly"
        [ Weekly.example1
        , Weekly.example2
        , Weekly.example3
        , Weekly.example4_1
        , Weekly.example4_2
        , Weekly.example5
        , Weekly.example6
        ]


monthly : Test
monthly =
    testExamples "Monthly"
        [ Monthly.example1
        , Monthly.example2
        , Monthly.example3
        , Monthly.example4
        , Monthly.example5
        , Monthly.example6
        , Monthly.example7
        , Monthly.example8
        , Monthly.example9
        , Monthly.example10
        , Monthly.example11
        ]


yearly : Test
yearly =
    testExamples "Yearly"
        [ Yearly.example1
        , Yearly.example2
        , Yearly.example3
        , Yearly.example4
        , Yearly.example5_1
        , Yearly.example5_2
        , Yearly.example5_3
        , Yearly.example6
        , Yearly.example7
        , Yearly.example8
        , Yearly.example9
        ]


custom : Test
custom =
    testExamples "Custom"
        [ Custom.example1
        , Custom.example2
        , Custom.example3
        , Custom.example4
        ]


testExamples : String -> List Example -> Test
testExamples tipe recurrenceTests =
    describe tipe (List.map testExample recurrenceTests)


testExample : Example -> Test
testExample { description, rrule, recurrence, dates } =
    describe description
        [ Test.test "Generator.run : Recurrence -> List Posix" <|
            \_ ->
                Generator.run recurrence
                    |> Expect.equal (List.map Time.millisToPosix dates)
        , Test.test "Decoder.toRecurrence : String -> Recurrence" <|
            \_ ->
                Decoder.run rrule
                    |> Expect.equal (Ok recurrence)
        ]
