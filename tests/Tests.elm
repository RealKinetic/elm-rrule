module Tests exposing (..)

import Examples.Daily as Daily
import Examples.Example exposing (Example)
import Examples.Monthly as Monthly
import Examples.Weekly as Weekly
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


testExamples : String -> List Example -> Test
testExamples tipe recurrenceTests =
    describe tipe (List.map testExample recurrenceTests)


testExample : Example -> Test
testExample { description, rrule, recurrence, dates } =
    Test.test description <|
        \_ ->
            Generator.run recurrence
                |> Expect.equal (List.map Time.millisToPosix dates)
