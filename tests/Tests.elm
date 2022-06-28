module Tests exposing (..)

import Examples.Between as Between
import Examples.Custom as Custom
import Examples.Daily as Daily
import Examples.Monthly as Monthly
import Examples.Types exposing (Example)
import Examples.Weekly as Weekly
import Examples.Yearly as Yearly
import Expect
import RRule
import Test exposing (Test, describe)
import Time exposing (Posix)



--daily : Test
--daily =
--    testExamples "Daily"
--        [ Daily.example1
--        , Daily.example2
--        , Daily.example3
--        , Daily.example4
--        , Daily.example5
--        ]


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



--
--custom : Test
--custom =
--    testExamples "Custom"
--        [ Custom.example1
--        , Custom.example2
--        , Custom.example3
--        , Custom.example4
--        , Custom.example5
--        , Custom.example6
--        , Custom.example7
--        , Custom.example8
--        , Custom.example9
--        , Custom.example10
--        , Custom.example11
--        , Custom.example12
--        ]
--
--
--suite : Test
--suite =
--    testBetweens
--        [ Between.example1
--        , Between.example2
--        , Between.example3
--        , Between.example4
--        , Between.example5
--        , Between.example6
--        , Between.example7
--        , Between.example8_1
--        , Between.example8_2
--        , Between.example8_3
--        , Between.example9
--        , Between.example10
--        , Between.example11
--        , Between.example12
--        , Between.example13
--        ]


testExamples : String -> List Example -> Test
testExamples tipe recurrenceTests =
    describe tipe (List.map testExample recurrenceTests)


testExample : Example -> Test
testExample { description, rrule, recurrence, dates } =
    describe description
        [ --Test.test "RRule.all" <|
          --    \_ ->
          --        RRule.all recurrence
          --            |> Expect.equal (List.map Time.millisToPosix dates)
          --, Test.test "RRule.fromStrings" <|
          --    \_ ->
          --        RRule.fromStrings rrule
          --            |> Expect.equal (Ok recurrence)
          --,
          Test.test "RRule.toText" <|
            \_ ->
                RRule.toText recurrence |> Expect.equal ""
        ]


testBetweens : List Between.Example -> Test
testBetweens betweenTests =
    describe "RRule.between" (List.map testBetween betweenTests)


testBetween : Between.Example -> Test
testBetween { description, recurrence, window, dates } =
    Test.test description <|
        \_ ->
            RRule.between window recurrence
                |> Expect.equal (List.map Time.millisToPosix dates)
