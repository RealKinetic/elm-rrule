module Weekly exposing (suite)

import Either exposing (Either(..))
import Expect
import Interval.Weekly as Weekly
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Test exposing (..)
import Time exposing (Weekday(..))
import TimeZone


{-| Tests are based on the examples in the iCalendar Spec
<https://tools.ietf.org/html/rfc5545#section-3.8.5.3>
-}
suite : Test
suite =
    [ test1, test2, test3, test4_1, test4_2, test5, test6 ]
        |> List.map toTest
        |> describe "Weekly Tests"


type alias RecurrenceTest =
    { description : String
    , rrule : List String
    , recurrence : Recurrence
    , dates : List Int
    }


toTest : RecurrenceTest -> Test
toTest { description, rrule, recurrence, dates } =
    test description <|
        \_ ->
            Weekly.generate recurrence
                |> Expect.equal (List.map Time.millisToPosix dates)


defaultRules : Recurrence
defaultRules =
    { weekStart = Mon
    , frequency = Weekly
    , interval = 1
    , dtStart = Time.millisToPosix 0
    , tzid = TimeZone.america__new_york ()
    , untilCount = Nothing
    , byDay = []
    , byWeekNo = []
    , byMonthDay = []
    , byMonth = []
    , exdates = Nothing
    }


{-| Weekly for 10 occurrences

     DTSTART;TZID=America/New_York:19970902T090000
     RRULE:FREQ=WEEKLY;COUNT=10

     (1997 9:00 AM EDT)September 2,9,16,23,30;October 7,14,21
     (1997 9:00 AM EST)October 28;November 4

-}
test1 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 873205200000
        }
    , dates =
        [ 873205200000
        , 873810000000
        , 874414800000
        , 875019600000
        , 875624400000
        , 876229200000
        , 876834000000
        , 877438800000
        , 878047200000
        , 878652000000
        ]
    }


{-| Weekly until December 24, 1997

     DTSTART;TZID=America/New_York:19970902T090000
     RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z

     (1997 9:00 AM EDT)September 2,9,16,23,30;October 7,14,21;
     (1997 9:00 AM EST)October 28;November 4,11,18,25;December 2,9,16,23;

-}
test2 =
    { description = "Weekly until December 24, 1997"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 882921600000)
            , dtStart = Time.millisToPosix 873205200000
        }
    , dates =
        [ 873205200000
        , 873810000000
        , 874414800000
        , 875019600000
        , 875624400000
        , 876229200000
        , 876834000000
        , 877438800000
        , 878047200000
        , 878652000000
        , 879256800000
        , 879861600000
        , 880466400000
        , 881071200000
        , 881676000000
        , 882280800000
        , 882885600000
        ]
    }


{-| Every other week 11 occurences:

    DTSTART;TZID=US-Eastern:19970902T090000
    RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU

    (1997 9:00 AM EDT)September 2,16,30;October 14,28;
    (1997 9:00 AM EDT)November 11,25;December 9,23
    (1998 9:00 AM EST)January 6,20;

-}
test3 =
    { description = "Every other week 11 occurences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU;COUNT=11"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 11)
            , dtStart = Time.millisToPosix 873205200000
            , weekStart = Sun
            , interval = 2
        }
    , dates =
        [ 873205200000
        , 874414800000
        , 875624400000
        , 876834000000
        , 878047200000
        , 879256800000
        , 880466400000
        , 881676000000
        , 882885600000
        , 884095200000
        , 885304800000
        ]
    }


{-| Weekly on Tuesday and Thursday for 5 weeks:

DTSTART;TZID=US-Eastern:19970902T090000
RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH

(1997 9:00 AM EDT)September 2,4,9,11,16,18,23,25,30;October 2

-}
test4_1 =
    { description = "Weekly on Tuesday and Thursday for 5 weeks w/ UNTIL"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 876182400000)
            , dtStart = Time.millisToPosix 873205200000
            , weekStart = Sun
            , byDay = [ Right Tue, Right Thu ]
        }
    , dates =
        [ 873205200000
        , 873378000000
        , 873810000000
        , 873982800000
        , 874414800000
        , 874587600000
        , 875019600000
        , 875192400000
        , 875624400000
        , 875797200000
        ]
    }


{-| Weekly on Tuesday and Thursday for 5 weeks:

DTSTART;TZID=US-Eastern:19970902T090000
RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH

(1997 9:00 AM EDT)September 2,4,9,11,16,18,23,25,30;October 2

-}
test4_2 =
    { description = "Weekly on Tuesday and Thursday for 5 weeks w/ COUNT"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 873205200000
            , weekStart = Sun
            , byDay = [ Right Tue, Right Thu ]
        }
    , dates =
        [ 873205200000
        , 873378000000
        , 873810000000
        , 873982800000
        , 874414800000
        , 874587600000
        , 875019600000
        , 875192400000
        , 875624400000
        , 875797200000
        ]
    }


{-| Every other week on Monday, Wednesday and Friday until December 24,
1997, but starting on Tuesday, September 2, 1997:

    DTSTART;TZID=US-Eastern:19970902T090000
    RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR
    (1997 9:00 AM EDT)September 2,3,5,15,17,19,29;October1,3,13,15,17
    (1997 9:00 AM EST)October 27,29,31;November 10,12,14,24,26,28;December 8,10,12,22

-}
test5 =
    { description = "Every other week on Monday, Wednesday and Friday until December 24, 1997, but starting on Tuesday, September 2, 1997"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 882921600000)
            , dtStart = Time.millisToPosix 873205200000
            , weekStart = Sun
            , byDay = [ Right Mon, Right Wed, Right Fri ]
            , interval = 2
        }
    , dates =
        [ 873291600000
        , 873464400000
        , 874328400000
        , 874501200000
        , 874674000000
        , 875538000000
        , 875710800000
        , 875883600000
        , 876747600000
        , 876920400000
        , 877093200000
        , 877960800000
        , 878133600000
        , 878306400000
        , 879170400000
        , 879343200000
        , 879516000000
        , 880380000000
        , 880552800000
        , 880725600000
        , 881589600000
        , 881762400000
        , 881935200000
        , 882799200000
        ]
    }


{-| Every other week on Tuesday and Thursday, for 8 occurrences:

    DTSTART;TZID=US-Eastern:19970902T090000
    RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH

    (1997 9:00 AM EDT)September 2,4,16,18,30;October 2,14,16

-}
test6 =
    { description = "Every other week on Tuesday and Thursday, for 8 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 8)
            , dtStart = Time.millisToPosix 873205200000
            , weekStart = Sun
            , byDay = [ Right Tue, Right Thu ]
            , interval = 2
        }
    , dates =
        [ 873205200000
        , 873378000000
        , 874414800000
        , 874587600000
        , 875624400000
        , 875797200000
        , 876834000000
        , 877006800000
        ]
    }
