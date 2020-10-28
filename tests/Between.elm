module Between exposing (..)

import Decoder
import Expect
import Generator
import Recurrence exposing (Recurrence)
import Test exposing (Test, describe)
import Time exposing (Posix)
import TimeZone


type alias BetweenTest =
    { description : String
    , recurrence : Result Decoder.Error Recurrence
    , start : Posix
    , end : Posix
    }


weekly1 : BetweenTest
weekly1 =
    { description = "One count weekly recurring outside of date range"
    , recurrence =
        Decoder.runWithDTSTART
            [ "RRULE:FREQ=WEEKLY;WKST=SU;COUNT=1;BYDAY=TH,TU,WE" ]
            (TimeZone.america__denver ())
            (Time.millisToPosix 1596560400000 {- 2020-08-04T17:00:00.000Z -})
    , start = (Time.millisToPosix 1603213200000 {- 2020-10-20T17:00:00.000Z -})
    , end = (Time.millisToPosix 1604077200000 {- 2020-10-30T17:00:00.000Z -})
    }


toTest : BetweenTest -> Test
toTest { description, recurrence, start, end } =
    Test.test description <|
        \_ ->
            recurrence
                |> Result.map (Generator.between { start = start, end = end })
                |> Expect.equal (Ok [])


suite : Test
suite =
    describe "Generator.between"
        (List.map toTest [ weekly1 ])
