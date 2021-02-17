module Examples.Custom exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), RRule, UntilCount(..))
import Time exposing (Weekday(..))
import TimeZone


defaultRules : RRule
defaultRules =
    { weekStart = Mon
    , frequency = Yearly
    , interval = 1
    , dtStart = Time.millisToPosix 0
    , tzid = TimeZone.america__denver ()
    , untilCount = Nothing
    , byDay = []
    , byWeekNo = []
    , byMonthDay = []
    , byMonth = []
    , byYearDay = []
    , exdates = []
    , rdates = []
    }


{-| Test All-Day events
"The value of the UNTIL rule part MUST have the same value type as the "DTSTART" property."
-}
example1 =
    { description = "Weekly All-Day event w/ All-Day UNTIL"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20200603"
        , "RRULE:FREQ=WEEKLY;UNTIL=20200825;BYDAY=WE"
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , untilCount = Just (Until <| Time.millisToPosix 1598335200000)
            , dtStart = Time.millisToPosix 1591164000000
            , byDay = [ Right Wed ]
        }
    , dates = [ 1591164000000, 1591768800000, 1592373600000, 1592978400000, 1593583200000, 1594188000000, 1594792800000, 1595397600000, 1596002400000, 1596607200000, 1597212000000, 1597816800000 ]
    }


{-| Test Yearly event with no by rules.

This ensures Generator.hasNoExpands is working properly, so it

-}
example2 =
    { description = "Yearly All-Day event with no by-rules. Also tests inclusivity of UNTIL"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20190314"
        , "RRULE:FREQ=YEARLY;UNTIL=20230314"
        ]
    , recurrence =
        { defaultRules
            | frequency = Yearly
            , untilCount = Just (Until <| Time.millisToPosix 1678773600000)
            , dtStart = Time.millisToPosix 1552543200000
        }
    , dates = [ 1552543200000, 1584165600000, 1615705200000, 1647237600000, 1678773600000 ]
    }


{-| -}
example3 =
    { description = "An issue seen on Robert's calendar. Don't know what fixed it."
    , rrule =
        [ "EXDATE;TZID=America/Denver:20190719T093000"
        , "RRULE:FREQ=WEEKLY;UNTIL=20190808T235959Z;BYDAY=FR"
        , "DTSTART;TZID=America/Denver:20190531T093000"
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , untilCount = Just (Until <| Time.millisToPosix 1565308799000)
            , dtStart = Time.millisToPosix 1559316600000
            , byDay = [ Right Fri ]
            , exdates = [ Time.millisToPosix 1563550200000 ]
        }
    , dates = [ 1559316600000, 1559921400000, 1560526200000, 1561131000000, 1561735800000, 1562340600000, 1562945400000, 1564155000000, 1564759800000 ]
    }


{-| Helps handle Google Calendar's out of spec "All-Day -> Timed soft-master UNTIL inheritence" issue.

According to the spec the UNITL and DSTART should be the same time value type.

-}
example4 =
    { description = "All-Day converted to Timed event (with inherited All-Day UNTIL)"
    , rrule =
        [ "RRULE:FREQ=MONTHLY;UNTIL=20210611;INTERVAL=2;BYDAY=2TU"
        , "DTSTART;TZID=America/Denver:20201208T093000"
        ]
    , recurrence =
        { defaultRules
            | frequency = Monthly
            , untilCount = Just (Until <| Time.millisToPosix 1623391200000)
            , interval = 2
            , dtStart = Time.millisToPosix 1607445000000
            , byDay = [ Left ( 2, Tue ) ]
        }
    , dates = [ 1607445000000, 1612888200000, 1618327800000, 1623166200000 ]
    }


{-| EXDATES should be removed after initial instances are generated.
This way additional instances aren't generated past the COUNT.
-}
example5 =
    { description = "EXDATES not counted for/against COUNT"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20190718T143000"
        , "EXDATE;TZID=America/Denver:20190815T143000,20191107T143000"
        , "RRULE:FREQ=WEEKLY;WKST=SU;COUNT=18;INTERVAL=2;BYDAY=TH"
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , weekStart = Sun
            , untilCount = Just (Count 18)
            , interval = 2
            , byDay = [ Right Thu ]
            , dtStart = Time.millisToPosix 1563481800000
            , exdates = [ Time.millisToPosix 1565901000000, Time.millisToPosix 1573162200000 ]
        }
    , dates = [ 1563481800000, 1564691400000, 1567110600000, 1568320200000, 1569529800000, 1570739400000, 1571949000000, 1574371800000, 1575581400000, 1576791000000, 1578000600000, 1579210200000, 1580419800000, 1581629400000, 1582839000000, 1584045000000 ]
    }


{-| Test trimming rrule strings
-}
example6 =
    { description = "RRULE strings with spaces & newlines"
    , rrule =
        [ "  DTSTART;TZID=America/Denver:20200603 \n "
        , "  \n RRULE:FREQ=WEEKLY;UNTIL=20200825;BYDAY=WE  \n  "
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , untilCount = Just (Until <| Time.millisToPosix 1598335200000)
            , dtStart = Time.millisToPosix 1591164000000
            , byDay = [ Right Wed ]
        }
    , dates = [ 1591164000000, 1591768800000, 1592373600000, 1592978400000, 1593583200000, 1594188000000, 1594792800000, 1595397600000, 1596002400000, 1596607200000, 1597212000000, 1597816800000 ]
    }


{-| Le birthday
-}
example7 =
    { description = "Infer the day/month in a yearly event. No byrules."
    , rrule =
        [ "DTSTART;TZID=America/Denver:19870701T000000"
        , "RRULE:FREQ=YEARLY;COUNT=3"
        ]
    , recurrence =
        { defaultRules
            | untilCount = Just (Count 3)
            , dtStart = Time.millisToPosix 552117600000
        }
    , dates = [ 552117600000, 583740000000, 615276000000 ]
    }


{-| Infer BYMONTHDAY on MONTHLY

Checks the "31st of the month bug" where not every month has a 31st day.

-}
example8 =
    { description = "Infer the monthday in a monthly event."
    , rrule =
        [ "DTSTART;TZID=America/Denver:20210731T160000"
        , "RRULE:FREQ=MONTHLY;COUNT=5"
        ]
    , recurrence =
        { defaultRules
            | frequency = Monthly
            , untilCount = Just (Count 5)
            , dtStart = Time.millisToPosix 1627768800000
        }
    , dates = [ 1627768800000, 1630447200000, 1635717600000, 1640991600000, 1643670000000 ]
    }


{-| Every Friday the 13th, forever:
Alternative apporach to Monthly.example10 "Every Friday the 13th"
-}
example9 =
    { description = "Every Friday the 13th"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970902T090000"
        , "RRULE:FREQ=YEARLY;BYDAY=FR;BYMONTHDAY=13;COUNT=10"
        ]
    , recurrence =
        { defaultRules
            | frequency = Yearly
            , tzid = TimeZone.america__new_york ()
            , untilCount = Just (Count 10)
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
    }
