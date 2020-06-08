module Examples.Daily exposing (..)

import Recurrence exposing (Frequency(..), Recurrence, UntilCount(..))
import Set
import Time exposing (Weekday(..))
import TimeZone


defaultRules : Recurrence
defaultRules =
    { weekStart = Mon
    , frequency = Daily
    , interval = 1
    , dtStart = Time.millisToPosix 873205200000
    , tzid = TimeZone.america__new_york ()
    , untilCount = Nothing
    , byDay = []
    , byWeekNo = []
    , byMonthDay = []
    , byMonth = []
    , byYearDay = []
    , exdates = Set.empty
    }


{-| Daily for 10 occurrences:

       DTSTART;TZID=America/New_York:19970902T090000
       RRULE:FREQ=DAILY;COUNT=10

       ==> (1997 9:00 AM EDT) September 2-11

-}
example1 =
    { description = "Daily for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=DAILY;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
        }
    , dates =
        [ 873205200000
        , 873291600000
        , 873378000000
        , 873464400000
        , 873550800000
        , 873637200000
        , 873723600000
        , 873810000000
        , 873896400000
        , 873982800000
        ]
    }


{-| Daily until November 15th, 1997:

    DTSTART;TZID=America/New_York:19970902T090000
    RRULE:FREQ=DAILY;UNTIL=19971115T000000Z

    ==> (1997 9:00 AM EDT) September 2-30;October 1-25
    (1997 9:00 AM EST) October 26-31;November 1-15

NOTE: Differs from original in spec by end slightly sooner UNTIL date.

-}
example2 =
    { description = "Daily until December 24, 1997"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=DAILY;UNTIL=19971115T000000Z"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 879570000000)
        }
    , dates =
        [ 873205200000, 873291600000, 873378000000, 873464400000, 873550800000, 873637200000, 873723600000, 873810000000, 873896400000, 873982800000, 874069200000, 874155600000, 874242000000, 874328400000, 874414800000, 874501200000, 874587600000, 874674000000, 874760400000, 874846800000, 874933200000, 875019600000, 875106000000, 875192400000, 875278800000, 875365200000, 875451600000, 875538000000, 875624400000, 875710800000, 875797200000, 875883600000, 875970000000, 876056400000, 876142800000, 876229200000, 876315600000, 876402000000, 876488400000, 876574800000, 876661200000, 876747600000, 876834000000, 876920400000, 877006800000, 877093200000, 877179600000, 877266000000, 877352400000, 877438800000, 877525200000, 877611600000, 877698000000, 877784400000, 877874400000, 877960800000, 878047200000, 878133600000, 878220000000, 878306400000, 878392800000, 878479200000, 878565600000, 878652000000, 878738400000, 878824800000, 878911200000, 878997600000, 879084000000, 879170400000, 879256800000, 879343200000, 879429600000, 879516000000 ]
    }


{-| Every other day - forever:

    DTSTART;TZID=America/New_York:19970902T090000
    RRULE:FREQ=DAILY;INTERVAL=2

    ==> (1997 9:00 AM EDT) September 2,4,6,8...24,26,28,30;
                          October 2,4,6...20,22,24
       (1997 9:00 AM EST) October 26,28,30;
                          November 1,3,5,7...25,27,29;
                          December 1,3,...

NOTE: Capped at 10 count

-}
example3 =
    { description = "Every other day"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=DAILY;INTERVAL=2;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , interval = 2
        }
    , dates =
        [ 873205200000
        , 873378000000
        , 873550800000
        , 873723600000
        , 873896400000
        , 874069200000
        , 874242000000
        , 874414800000
        , 874587600000
        , 874760400000
        ]
    }


{-| Every 10 days, 5 occurrences:

    DTSTART;TZID=America/New_York:19970902T090000
    RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5

    ==> (1997 9:00 AM EDT) September 2,12,22; October 2,12

-}
example4 =
    { description = "Every 10 days, 5 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 5)
            , interval = 10
        }
    , dates =
        [ 873205200000
        , 874069200000
        , 874933200000
        , 875797200000
        , 876661200000
        ]
    }


{-| Every day in January, for 3 years:

           DTSTART;TZID=America/New_York:19980101T090000
           RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1

           ==> (1998 9:00 AM EST)January 1-31
               (1999 9:00 AM EST)January 1-31
               (2000 9:00 AM EST)January 1-31

-}
example5 =
    { description = "Every day in January, for 3 years"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19980101T090000"
        , "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 949327200000)
            , dtStart = Time.millisToPosix 883663200000
            , byMonth = [ 1 ]
        }
    , dates =
        [ 883663200000, 883749600000, 883836000000, 883922400000, 884008800000, 884095200000, 884181600000, 884268000000, 884354400000, 884440800000, 884527200000, 884613600000, 884700000000, 884786400000, 884872800000, 884959200000, 885045600000, 885132000000, 885218400000, 885304800000, 885391200000, 885477600000, 885564000000, 885650400000, 885736800000, 885823200000, 885909600000, 885996000000, 886082400000, 886168800000, 886255200000, 915199200000, 915285600000, 915372000000, 915458400000, 915544800000, 915631200000, 915717600000, 915804000000, 915890400000, 915976800000, 916063200000, 916149600000, 916236000000, 916322400000, 916408800000, 916495200000, 916581600000, 916668000000, 916754400000, 916840800000, 916927200000, 917013600000, 917100000000, 917186400000, 917272800000, 917359200000, 917445600000, 917532000000, 917618400000, 917704800000, 917791200000, 946735200000, 946821600000, 946908000000, 946994400000, 947080800000, 947167200000, 947253600000, 947340000000, 947426400000, 947512800000, 947599200000, 947685600000, 947772000000, 947858400000, 947944800000, 948031200000, 948117600000, 948204000000, 948290400000, 948376800000, 948463200000, 948549600000, 948636000000, 948722400000, 948808800000, 948895200000, 948981600000, 949068000000, 949154400000, 949240800000, 949327200000 ]
    }


{-| Every 20 minutes from 9:00 AM to 4:40 PM every day:

    DTSTART;TZID=America/New_York:19970902T090000
    RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40
    or
    RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16

    ==> (September 2, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
                               ... 16:00,16:20,16:40
       (September 3, 1997 EDT) 9:00,9:20,9:40,10:00,10:20,
                               ...16:00,16:20,16:40
       ...

TODO Not yet supported

-}
test6 =
    { description = "Every 20 minutes from 9:00 AM to 4:40 PM every day"
    , rrule =
        [ ""
        , ""
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
        }
    , dates =
        []
    }
