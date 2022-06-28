module Examples.Monthly exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), RRule, UntilCount(..))
import Time exposing (Weekday(..))
import TimeZone


defaultRules : RRule
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
    , byYearDay = []
    , exdates = []
    , rdates = []
    }


{-| Monthly on the first Friday for 10 occurrences

    DTSTART;TZID=America/New_York:19970905T090000
    RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR

    (1997 9:00 AM EDT) September 5;October 3; November 7;December 5;
    January 2; February 6;March 6;April 3;May 1;June 5

-}
example1 =
    { description = "Monthly on the first Friday for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970905T090000"
        , "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , byDay = [ Left ( 1, Fri ) ]
            , dtStart = Time.millisToPosix 873464400000
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
    , text = ""
    }


{-| Monthly on the first Friday until December 24, 1997:

    DTSTART;TZID=America/New_York:19970905T090000
    RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR

    ==> (1997 9:00 AM EDT) September 5; October 3
    (1997 9:00 AM EST) November 7; December 5

-}
example2 =
    { description = "Monthly on the first Friday until December 24, 1997:"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970905T090000"
        , "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 882921600000)
            , byDay = [ Left ( 1, Fri ) ]
            , dtStart = Time.millisToPosix 873464400000
        }
    , dates =
        [ 873464400000
        , 875883600000
        , 878911200000
        , 881330400000
        ]
    , text = ""
    }


{-| Every other month on the first and last Sunday of the month for 10
occurrences:

    DTSTART;TZID=America/New_York:19970907T090000
    RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU

    September 7, 28; November 2, 30; January 4, 25; March 1, 29; May 3, 31

-}
example3 =
    { description = "Every other month on the first and last Sunday of the month for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970907T090000"
        , "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , interval = 2
            , byDay = [ Left ( 1, Sun ), Left ( -1, Sun ) ]
            , dtStart = Time.millisToPosix 873637200000
        }
    , dates =
        [ 873637200000
        , 875451600000
        , 878479200000
        , 880898400000
        , 883922400000
        , 885736800000
        , 888760800000
        , 891180000000
        , 894200400000
        , 896619600000
        ]
    , text = ""
    }


{-|

      Monthly on the second-to-last Monday of the month for 6 months:

       DTSTART;TZID=America/New_York:19970922T090000
       RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO

       ==> (1997 9:00 AM EDT) September 22;October 20
           (1997 9:00 AM EST) November 17;December 22
           (1998 9:00 AM EST) January 19;February 16

-}
example4 =
    { description = "Monthly on the second-to-last Monday of the month for 6 months"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970922T090000"
        , "RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 6)
            , dtStart = Time.millisToPosix 874933200000
            , byDay = [ Left ( -2, Mon ) ]
        }
    , dates =
        [ 874933200000
        , 877352400000
        , 879775200000
        , 882799200000
        , 885218400000
        , 887637600000
        ]
    , text = ""
    }


{-| Monthly on the third-to-the-last day of the month, forever:

        DTSTART;TZID=America/New_York:19970928T090000
        RRULE:FREQ=MONTHLY;BYMONTHDAY=-3;COUNT=50

           ==> (1997 9:00 AM EDT) September 28
               (1997 9:00 AM EST) October 29;November 28;December 29
               (1998 9:00 AM EST) January 29;February 26
               ...

NOTE: I cap this at 10 count

-}
example5 =
    { description = "Monthly on the third-to-the-last day of the month 50 times"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970928T090000"
        , "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 875451600000
            , byMonthDay = [ -3 ]
        }
    , dates =
        [ 875451600000
        , 878133600000
        , 880725600000
        , 883404000000
        , 886082400000
        , 888501600000
        , 891180000000
        , 893768400000
        , 896446800000
        , 899038800000
        ]
    , text = ""
    }


{-| Monthly on the 2nd and 15th of the month for 10 occurrences:

           DTSTART;TZID=America/New_York:19970902T090000
           RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15

           ==> (1997 9:00 AM EDT) September 2,15;October 2,15
               (1997 9:00 AM EST) November 2,15;December 2,15
               (1998 9:00 AM EST) January 2,15

-}
example6 =
    { description = "Monthly on the 2nd and 15th of the month for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 873205200000
            , byMonthDay = [ 2, 15 ]
        }
    , dates =
        [ 873205200000
        , 874328400000
        , 875797200000
        , 876920400000
        , 878479200000
        , 879602400000
        , 881071200000
        , 882194400000
        , 883749600000
        , 884872800000
        ]
    , text = ""
    }


{-| Monthly on the first and last day of the month for 10 occurrences:

           DTSTART;TZID=America/New_York:19970930T090000
           RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1

           ==> (1997 9:00 AM EDT) September 30;October 1
               (1997 9:00 AM EST) October 31;November 1,30;December 1,31
               (1998 9:00 AM EST) January 1,31;February 1

-}
example7 =
    { description = "Weekly for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970930T090000"
        , "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 875624400000
            , byMonthDay = [ 1, -1 ]
        }
    , dates =
        [ 875624400000
        , 875710800000
        , 878306400000
        , 878392800000
        , 880898400000
        , 880984800000
        , 883576800000
        , 883663200000
        , 886255200000
        , 886341600000
        ]
    , text = ""
    }


{-| Every 18 months on the 10th thru 15th of the month for 10 occurrences:

           DTSTART;TZID=America/New_York:19970910T090000
           RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15

           ==> (1997 9:00 AM EDT) September 10,11,12,13,14,15
               (1999 9:00 AM EST) March 10,11,12,13

-}
example8 =
    { description = "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970910T090000"
        , "RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , interval = 18
            , dtStart = Time.millisToPosix 873896400000
            , byMonthDay = [ 10, 11, 12, 13, 14, 15 ]
        }
    , dates =
        [ 873896400000
        , 873982800000
        , 874069200000
        , 874155600000
        , 874242000000
        , 874328400000
        , 921074400000
        , 921160800000
        , 921247200000
        , 921333600000
        ]
    , text = ""
    }


{-| Every Tuesday, every other month:

           DTSTART;TZID=America/New_York:19970902T090000
           RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU

           ==> (1997 9:00 AM EDT) September 2,9,16,23,30
               (1997 9:00 AM EST) November 4,11,18,25
               (1998 9:00 AM EST) January 6,13,20,27;March 3,10,17,24,31
               ...

NOTE: I cap this at 10 count

-}
example9 =
    { description = "Every Tuesday, every other month"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , interval = 2
            , dtStart = Time.millisToPosix 873205200000
            , byDay = [ Right Tue ]
        }
    , dates =
        [ 873205200000
        , 873810000000
        , 874414800000
        , 875019600000
        , 875624400000
        , 878652000000
        , 879256800000
        , 879861600000
        , 880466400000
        , 884095200000
        ]
    , text = ""
    }


{-| Every Friday the 13th, forever:

           DTSTART;TZID=America/New_York:19970902T090000
           EXDATE;TZID=America/New_York:19970902T090000
           RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13

           ==> (1998 9:00 AM EST) February 13;March 13;November 13
               (1999 9:00 AM EDT) August 13
               (2000 9:00 AM EDT) October 13
               ...

NOTE: I cap this at 10 count

-}
example10 =
    { description = "Every Friday the 13th"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 873205200000
            , byMonthDay = [ 13 ]
            , byDay = [ Right Fri ]
        }
    , dates =
        [ 887378400000
        , 889797600000
        , 910965600000
        , 934549200000
        , 971442000000
        , 987166800000
        , 995029200000
        , 1031922000000
        , 1039788000000
        , 1055509200000
        ]
    , text = ""
    }


{-| The first Saturday that follows the first Sunday of the month,
forever:

           DTSTART;TZID=America/New_York:19970913T090000
           RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13

           ==> (1997 9:00 AM EDT) September 13;October 11
               (1997 9:00 AM EST) November 8;December 13
               (1998 9:00 AM EST) January 10;February 7;March 7
               (1998 9:00 AM EDT) April 11;May 9;June 13...
               ...

NOTE: I cap this at 10 count

-}
example11 =
    { description = "The first Saturday that follows the first Sunday of the month"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970913T090000"
        , "RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 874155600000
            , byMonthDay = [ 7, 8, 9, 10, 11, 12, 13 ]
            , byDay = [ Right Sat ]
        }
    , dates =
        [ 874155600000
        , 876574800000
        , 878997600000
        , 882021600000
        , 884440800000
        , 886860000000
        , 889279200000
        , 892299600000
        , 894718800000
        , 897742800000
        ]
    , text = ""
    }
