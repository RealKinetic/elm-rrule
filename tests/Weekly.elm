module Weekly exposing (suite)

import Either exposing (Either(..))
import Expect
import Interval.Weekly as Weekly
import RRule exposing (Frequency(..), UntilCount(..))
import Test exposing (..)
import Time exposing (Weekday(..))
import TimeZone


{-| 1997 9:00 AM EST
September 2,3,5,15,17,19,29;October 1,3,13,15,17,27,29,31;
October November 10,12,14,24,26,28;December 8,10,12,22;

Original version of test.

TODO write node script to quickly generate dates in the correct timezone

-}
everyOtherWeek : List String
everyOtherWeek =
    [ "DTSTART;TZID=US-Eastern:19970902T090000"
    , "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR\nDTSTART:19970902T090000"
    ]


suite : Test
suite =
    describe "Weekly Interval"
        [ test "Every Monday and Friday for 10 occurences at 9:30am in America/Denver" <|
            \_ ->
                Expect.equal
                    -- [ 'DTSTART;TZID=America/Denver:20200224T093000', 'RRULE:FREQ=WEEKLY;COUNT=10;BYDAY=MO,FR' ]
                    (Weekly.generate
                        { weekStart = Mon
                        , frequency = Weekly
                        , interval = 1
                        , dtStart = Time.millisToPosix 1582561800000
                        , tzid = TimeZone.america__denver ()
                        , untilCount = Just (Count 10)
                        , byDay = [ Right Mon, Right Fri ]
                        , byWeekNo = []
                        , byMonthDay = []
                        , byMonth = []
                        , exdates = Nothing
                        }
                    )
                    ([ 1582561800000
                     , 1582907400000
                     , 1583166600000
                     , 1583512200000
                     , 1583767800000
                     , 1584113400000
                     , 1584372600000
                     , 1584718200000
                     , 1584977400000
                     , 1585323000000
                     ]
                        |> List.map Time.millisToPosix
                    )
        , test "Every other week on Monday, Wednesday and Friday until December 24, 1997, but starting on Tuesday, September 2, 1997" <|
            \_ ->
                -- TODO Replace when parser is ready
                -- RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR\nDTSTART:19970902T090000
                Weekly.generate
                    { weekStart = Sun
                    , frequency = Weekly
                    , interval = 2
                    , dtStart = Time.millisToPosix 873190800000
                    , tzid = Time.utc
                    , untilCount = Just (Until <| Time.millisToPosix 882921600000)
                    , byDay = [ Right Mon, Right Wed, Right Fri ]
                    , byWeekNo = []
                    , byMonthDay = []
                    , byMonth = []
                    , exdates = Nothing
                    }
                    |> Expect.equal
                        ([ 873277200000
                         , 873450000000
                         , 874314000000
                         , 874486800000
                         , 874659600000
                         , 875523600000
                         , 875696400000
                         , 875869200000
                         , 876733200000
                         , 876906000000
                         , 877078800000
                         , 877942800000
                         , 878115600000
                         , 878288400000
                         , 879152400000
                         , 879325200000
                         , 879498000000
                         , 880362000000
                         , 880534800000
                         , 880707600000
                         , 881571600000
                         , 881744400000
                         , 881917200000
                         , 882781200000
                         ]
                            |> List.map Time.millisToPosix
                        )
        ]
