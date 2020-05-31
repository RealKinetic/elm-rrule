module Monthly exposing (..)

import Common
import Either exposing (Either(..))
import Interval.Monthly as Monthly
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Test exposing (..)
import Time exposing (Weekday(..))
import TimeZone


suite : Test
suite =
    [ test1, test2 ]
        |> List.map (Common.toTest Monthly.generate)
        |> describe "Monthly Tests"


defaultRules : Recurrence
defaultRules =
    { weekStart = Mon
    , frequency = Monthly
    , interval = 1
    , dtStart = Time.millisToPosix 873205200000
    , tzid = TimeZone.america__new_york ()
    , untilCount = Nothing
    , byDay = []
    , byWeekNo = []
    , byMonthDay = []
    , byMonth = []
    , exdates = Nothing
    }


{-| Monthly on the first Friday for 10 occurrences

    DTSTART;TZID=America/New_York:19970905T090000
    RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR

    (1997 9:00 AM EDT) September 5;October 3; November 7;December 5;January 2;
    February 6;March 6;April 3;May 1;June 5

-}
test1 =
    { description = "Monthly on the first Friday for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970905T090000"
        , "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , byDay = [ Left ( 1, Fri ) ]
        }
    , dates =
        [ 873464400000
        , 875883600000
        , 878911200000
        , 881330400000
        , 883749600000
        , 886773600000
        , 889192800000
        , 891612000000
        , 894027600000
        , 897051600000
        ]
    }


{-| Monthly on the first Friday until December 24, 1997:

    DTSTART;TZID=America/New_York:19970905T090000
    RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR

    ==> (1997 9:00 AM EDT) September 5; October 3
    (1997 9:00 AM EST) November 7; December 5

-}
test2 =
    { description = "Monthly on the first Friday until December 24, 1997:"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970905T090000"
        , "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 882921600000)
            , byDay = [ Left ( 1, Fri ) ]
        }
    , dates =
        [ 873464400000
        , 875883600000
        , 878911200000
        , 881330400000
        ]
    }


{-| Every other month on the first and last Sunday of the month for 10
occurrences:

    DTSTART;TZID=America/New_York:19970907T090000
    RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU

    September 7, 28; November 2, 30; January 4, 25; March 1, 29; May 3, 31

-}
test3 =
    { description = "Every other month on the first and last Sunday of the month for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970907T090000"
        , "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
        }
    , dates =
        []
    }


{-|

      Monthly on the second-to-last Monday of the month for 6 months:

       DTSTART;TZID=America/New_York:19970922T090000
       RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO

       ==> (1997 9:00 AM EDT) September 22;October 20
           (1997 9:00 AM EST) November 17;December 22
           (1998 9:00 AM EST) January 19;February 16

-}
test4 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
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


{-| Monthly on the third-to-the-last day of the month, forever:

           DTSTART;TZID=America/New_York:19970928T090000
           RRULE:FREQ=MONTHLY;BYMONTHDAY=-3

           ==> (1997 9:00 AM EDT) September 28
               (1997 9:00 AM EST) October 29;November 28;December 29
               (1998 9:00 AM EST) January 29;February 26
               ...

-}
test5 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
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


{-| Monthly on the 2nd and 15th of the month for 10 occurrences:

           DTSTART;TZID=America/New_York:19970902T090000
           RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15

           ==> (1997 9:00 AM EDT) September 2,15;October 2,15
               (1997 9:00 AM EST) November 2,15;December 2,15
               (1998 9:00 AM EST) January 2,15

-}
test6 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
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


{-| Monthly on the first and last day of the month for 10 occurrences:

           DTSTART;TZID=America/New_York:19970930T090000
           RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1

           ==> (1997 9:00 AM EDT) September 30;October 1
               (1997 9:00 AM EST) October 31;November 1,30;December 1,31
               (1998 9:00 AM EST) January 1,31;February 1

-}
test7 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=WEEKLY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
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



--{-| Every 18 months on the 10th thru 15th of the month for 10
--occurrences:
--
--           DTSTART;TZID=America/New_York:19970910T090000
--           RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,
--            13,14,15
--
--           ==> (1997 9:00 AM EDT) September 10,11,12,13,14,15
--               (1999 9:00 AM EST) March 10,11,12,13
--
---}
--
--
--{-| Every Tuesday, every other month:
--
--           DTSTART;TZID=America/New_York:19970902T090000
--           RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU
--
--           ==> (1997 9:00 AM EDT) September 2,9,16,23,30
--               (1997 9:00 AM EST) November 4,11,18,25
--               (1998 9:00 AM EST) January 6,13,20,27;March 3,10,17,24,31
--               ...
--
---}
--
--
--{-| Every Friday the 13th, forever:
--
--           DTSTART;TZID=America/New_York:19970902T090000
--           EXDATE;TZID=America/New_York:19970902T090000
--           RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13
--
--           ==> (1998 9:00 AM EST) February 13;March 13;November 13
--               (1999 9:00 AM EDT) August 13
--               (2000 9:00 AM EDT) October 13
--               ...
--
---}
--
--
--{-| The first Saturday that follows the first Sunday of the month,
--forever:
--
--           DTSTART;TZID=America/New_York:19970913T090000
--           RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13
--
--           ==> (1997 9:00 AM EDT) September 13;October 11
--               (1997 9:00 AM EST) November 8;December 13
--               (1998 9:00 AM EST) January 10;February 7;March 7
--               (1998 9:00 AM EDT) April 11;May 9;June 13...
--               ...
--
---}
