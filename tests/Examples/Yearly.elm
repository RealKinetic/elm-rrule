module Examples.Yearly exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), RRule, UntilCount(..))
import Time exposing (Weekday(..))
import TimeZone


defaultRules : RRule
defaultRules =
    { weekStart = Mon

    -- TODO Test some Sunday weekstarts
    , frequency = Yearly
    , interval = 1
    , dtStart = Time.millisToPosix 0
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


{-| Every day in January, for 3 years:

    DTSTART;TZID=America/New_York:19980101T090000

    RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;
    BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA

    (1998 9:00 AM EST)January 1-31
    (1999 9:00 AM EST)January 1-31
    (2000 9:00 AM EST)January 1-31

-}
example1 =
    { description = "Every day in January, for 3 years"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19980101T090000"
        , "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Until <| Time.millisToPosix 949327200000)
            , dtStart = Time.millisToPosix 883663200000
            , byDay =
                [ Right Sun
                , Right Mon
                , Right Tue
                , Right Wed
                , Right Thu
                , Right Fri
                , Right Sat
                ]
            , byMonth = [ 1 ]
        }
    , dates = [ 883663200000, 883749600000, 883836000000, 883922400000, 884008800000, 884095200000, 884181600000, 884268000000, 884354400000, 884440800000, 884527200000, 884613600000, 884700000000, 884786400000, 884872800000, 884959200000, 885045600000, 885132000000, 885218400000, 885304800000, 885391200000, 885477600000, 885564000000, 885650400000, 885736800000, 885823200000, 885909600000, 885996000000, 886082400000, 886168800000, 886255200000, 915199200000, 915285600000, 915372000000, 915458400000, 915544800000, 915631200000, 915717600000, 915804000000, 915890400000, 915976800000, 916063200000, 916149600000, 916236000000, 916322400000, 916408800000, 916495200000, 916581600000, 916668000000, 916754400000, 916840800000, 916927200000, 917013600000, 917100000000, 917186400000, 917272800000, 917359200000, 917445600000, 917532000000, 917618400000, 917704800000, 917791200000, 946735200000, 946821600000, 946908000000, 946994400000, 947080800000, 947167200000, 947253600000, 947340000000, 947426400000, 947512800000, 947599200000, 947685600000, 947772000000, 947858400000, 947944800000, 948031200000, 948117600000, 948204000000, 948290400000, 948376800000, 948463200000, 948549600000, 948636000000, 948722400000, 948808800000, 948895200000, 948981600000, 949068000000, 949154400000, 949240800000, 949327200000 ]
    , text = ""
    }


{-| Yearly in June and July for 10 occurrences:

    DTSTART;TZID=America/New_York:19970610T090000
    RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7

    (1997 9:00 AM EDT) June 10;July 10
    (1998 9:00 AM EDT) June 10;July 10
    (1999 9:00 AM EDT) June 10;July 10
    (2000 9:00 AM EDT) June 10;July 10
    (2001 9:00 AM EDT) June 10;July 10

    Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
    components are specified, the day is gotten from "DTSTART".

-}
example2 =
    { description = "Yearly in June and July for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970610T090000"
        , "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 865947600000
            , byMonth = [ 6, 7 ]
        }
    , dates =
        [ 865947600000
        , 868539600000
        , 897483600000
        , 900075600000
        , 929019600000
        , 931611600000
        , 960642000000
        , 963234000000
        , 992178000000
        , 994770000000
        ]
    , text = ""
    }


{-| Every other year on January, February, and March for 10 occurrences:

    DTSTART;TZID=America/New_York:19970310T090000
    RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3

    (1997 9:00 AM EST) March 10
    (1999 9:00 AM EST) January 10;February 10;March 10
    (2001 9:00 AM EST) January 10;February 10;March 10
    (2003 9:00 AM EST) January 10;February 10;March 10

-}
example3 =
    { description = "Every other year on January, February, and March for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970310T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 858002400000
            , interval = 2
            , byMonth = [ 1, 2, 3 ]
        }
    , dates =
        [ 858002400000
        , 915976800000
        , 918655200000
        , 921074400000
        , 979135200000
        , 981813600000
        , 984232800000
        , 1042207200000
        , 1044885600000
        , 1047304800000
        ]
    , text = ""
    }


{-| Every third year on the 1st, 100th, and 200th day for 10 occurrences:

    DTSTART;TZID=America/New_York:19970101T090000
    RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200

    (1997 9:00 AM EST) January 1
    (1997 9:00 AM EDT) April 10;July 19
    (2000 9:00 AM EST) January 1
    (2000 9:00 AM EDT) April 9;July 18
    (2003 9:00 AM EST) January 1
    (2003 9:00 AM EDT) April 10;July 19
    (2006 9:00 AM EST) January 1

-}
example4 =
    { description = "Every third year on the 1st, 100th, and 200th day for 10 occurrences"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970101T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 852127200000
            , interval = 3
            , byYearDay = [ 1, 100, 200 ]
        }
    , dates =
        [ 852127200000
        , 860677200000
        , 869317200000
        , 946735200000
        , 955285200000
        , 963925200000
        , 1041429600000
        , 1049979600000
        , 1058619600000
        , 1136124000000
        ]
    , text = ""
    }


{-| Every 20th Monday of the year, forever:

    DTSTART;TZID=America/New_York:1997 05 19 T090000
    RRULE:FREQ=YEARLY;BYDAY=20MO

    (1997 9:00 AM EDT) May 19
    (1998 9:00 AM EDT) May 18
    (1999 9:00 AM EDT) May 17
    ...

-}
example5_1 =
    { description = "Every 20th Monday of the year w/ Monday week start (capped at 20)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970519T090000"
        , "RRULE:FREQ=YEARLY;BYDAY=20MO;COUNT=20"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 20)
            , dtStart = Time.millisToPosix 864046800000
            , byDay = [ Left ( 20, Mon ) ]
        }
    , dates =
        [ 864046800000
        , 895496400000
        , 926946000000
        , 958395600000
        , 989845200000
        , 1021899600000
        , 1053349200000
        , 1084798800000
        , 1116248400000
        , 1147698000000
        , 1179147600000
        , 1211202000000
        , 1242651600000
        , 1274101200000
        , 1305550800000
        , 1337000400000
        , 1369054800000
        , 1400504400000
        , 1431954000000
        , 1463403600000
        ]
    , text = ""
    }


example5_2 =
    { description = "Every 20th Monday of the year, w/ Sunday week start (capped at 20)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970519T090000"
        , "RRULE:FREQ=YEARLY;BYDAY=20MO;COUNT=20;WKST=SU"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 20)
            , dtStart = Time.millisToPosix 864046800000
            , byDay = [ Left ( 20, Mon ) ]
            , weekStart = Sun
        }
    , dates =
        [ 864046800000
        , 895496400000
        , 926946000000
        , 958395600000
        , 989845200000
        , 1021899600000
        , 1053349200000
        , 1084798800000
        , 1116248400000
        , 1147698000000
        , 1179147600000
        , 1211202000000
        , 1242651600000
        , 1274101200000
        , 1305550800000
        , 1337000400000
        , 1369054800000
        , 1400504400000
        , 1431954000000
        , 1463403600000
        ]
    , text = ""
    }


example5_3 =
    { description = "Every 20th Sunday of the year, w/ Sunday week start (capped at 20)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970518T090000"
        , "RRULE:FREQ=YEARLY;BYDAY=20SU;COUNT=20;WKST=SU"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 20)
            , dtStart = Time.millisToPosix 863960400000
            , byDay = [ Left ( 20, Sun ) ]
            , weekStart = Sun
        }
    , dates =
        [ 863960400000
        , 895410000000
        , 926859600000
        , 958309200000
        , 990363600000
        , 1021813200000
        , 1053262800000
        , 1084712400000
        , 1116162000000
        , 1147611600000
        , 1179666000000
        , 1211115600000
        , 1242565200000
        , 1274014800000
        , 1305464400000
        , 1336914000000
        , 1368968400000
        , 1400418000000
        , 1431867600000
        , 1463317200000
        ]
    , text = ""
    }


{-| Monday of week number 20 (where the default start of the week is
Monday), forever:

    DTSTART;TZID=America/New_York:19970512T090000
    RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO

    (1997 9:00 AM EDT) May 12
    (1998 9:00 AM EDT) May 11
    (1999 9:00 AM EDT) May 17
    ...

TODO Add with WKST=SU

-}
example6 =
    { description = "Monday of week number 20 (where the default start of the week is Monday, capped at 3)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970512T090000"
        , "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO;COUNT=3"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 3)
            , dtStart = Time.millisToPosix 863442000000
            , byWeekNo = [ 20 ]
            , byDay = [ Right Mon ]
        }
    , dates = [ 863442000000, 894891600000, 926946000000 ]
    , text = ""
    }


{-| Every Thursday in March, forever:

    DTSTART;TZID=America/New_York:19970313T090000
    RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH

    (1997 9:00 AM EST) March 13,20,27
    (1998 9:00 AM EST) March 5,12,19,26
    (1999 9:00 AM EST) March 4,11,18,25
    ...

-}
example7 =
    { description = "Every Thursday in March (capped at 10)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970313T090000"
        , "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 10)
            , dtStart = Time.millisToPosix 858261600000
            , byMonth = [ 3 ]
            , byDay = [ Right Thu ]
        }
    , dates =
        [ 858261600000
        , 858866400000
        , 859471200000
        , 889106400000
        , 889711200000
        , 890316000000
        , 890920800000
        , 920556000000
        , 921160800000
        , 921765600000
        ]
    , text = ""
    }


{-| Every Thursday, but only during June, July, and August, forever:

    DTSTART;TZID=America/New_York:19970605T090000
    RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8

    (1997 9:00 AM EDT) June 5,12,19,26;July 3,10,17,24,31;
    August 7,14,21,28
    (1998 9:00 AM EDT) June 4,11,18,25;July 2,9,16,23,30;
    August 6,13,20,27
    (1999 9:00 AM EDT) June 3,10,17,24;July 1,8,15,22,29;
    August 5,12,19,26
    ...

-}
example8 =
    { description = "Every Thursday, but only during June, July, and August (capped at 20)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970605T090000"
        , "RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8;COUNT=20"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 20)
            , dtStart = Time.millisToPosix 865515600000
            , byDay = [ Right Thu ]
            , byMonth = [ 6, 7, 8 ]
        }
    , dates =
        [ 865515600000
        , 866120400000
        , 866725200000
        , 867330000000
        , 867934800000
        , 868539600000
        , 869144400000
        , 869749200000
        , 870354000000
        , 870958800000
        , 871563600000
        , 872168400000
        , 872773200000
        , 896965200000
        , 897570000000
        , 898174800000
        , 898779600000
        , 899384400000
        , 899989200000
        , 900594000000
        ]
    , text = ""
    }


{-| Every 4 years, the first Tuesday after a Monday in November,
forever (U.S. Presidential Election day):

    DTSTART;TZID=America/New_York:19961105T090000
    RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8

    (1996 9:00 AM EST) November 5
    (2000 9:00 AM EST) November 7
    (2004 9:00 AM EST) November 2
    ...

-}
example9 =
    { description = "Every 4 years, the first Tuesday after a Monday in November (U.S. Presidential Election day, capped at 3)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19961105T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8;COUNT=3"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 3)
            , dtStart = Time.millisToPosix 847202400000
            , interval = 4
            , byMonth = [ 11 ]
            , byDay = [ Right Tue ]
            , byMonthDay = [ 2, 3, 4, 5, 6, 7, 8 ]
        }
    , dates = [ 847202400000, 973605600000, 1099404000000 ]
    , text = ""
    }
