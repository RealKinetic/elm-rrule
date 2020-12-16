module Examples.Between exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), RRule, UntilCount(..))
import Time exposing (Posix, Weekday(..))
import TimeZone


type alias Example =
    { description : String
    , rrule : List String
    , recurrence : RRule
    , window : { start : Posix, end : Posix }
    , dates : List Int
    }


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


example1 : Example
example1 =
    { description = "One count weekly recurring outside of date range"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20200804T110000"
        , "RRULE:FREQ=WEEKLY;WKST=SU;COUNT=1;BYDAY=TH,TU,WE"
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , weekStart = Mon
            , untilCount = Just (Count 1)
            , byDay = [ Right Thu, Right Tue, Right Wed ]
            , dtStart = Time.millisToPosix 1596560400000
        }
    , window =
        { start = (Time.millisToPosix 1603213200000 {- 2020-10-20T17:00:00.000Z -})
        , end = (Time.millisToPosix 1604077200000 {- 2020-10-30T17:00:00.000Z -})
        }
    , dates = []
    }


example2 : Example
example2 =
    { description = "window.start after DTSTART, UNTIL before window.end"
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
    , window =
        { start = (Time.millisToPosix 1561939200000 {- 2019-07-01T00:00:00.000Z -})
        , end = (Time.millisToPosix 1604077200000 {- 2020-10-30T17:00:00.000Z -})
        }
    , dates = [ 1562340600000, 1562945400000, 1564155000000, 1564759800000 ]
    }


example3 : Example
example3 =
    { description = "window.start after DTSTART, window.end before UNTIL"
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
    , window =
        { start = (Time.millisToPosix 1561939200000 {- 2019-07-01T00:00:00.000Z -})
        , end = (Time.millisToPosix 1563931301023 {- 2019-07-23T01:21:41.023Z -})
        }
    , dates = [ 1562340600000, 1562945400000 ]
    }


example4 : Example
example4 =
    { description = "window.start well before DTSTART, window.end well after UNTIL"
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
    , window =
        { start = (Time.millisToPosix 1361939200000 {- 2013-02-27T04:26:40.000Z -})
        , end = (Time.millisToPosix 1863931301023 {- 2029-01-23T11:41:41.023Z -})
        }
    , dates = [ 1559316600000, 1559921400000, 1560526200000, 1561131000000, 1561735800000, 1562340600000, 1562945400000, 1564155000000, 1564759800000 ]
    }


example5 : Example
example5 =
    { description = "Every other Thursday"
    , rrule =
        [ "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20200101T055959Z;INTERVAL=2;BYDAY=TH"
        , "DTSTART;TZID=America/Chicago:20190207T100000"
        ]
    , recurrence =
        { defaultRules
            | frequency = Weekly
            , weekStart = Sun
            , untilCount = Just (Until <| Time.millisToPosix 1577858399000)
            , byDay = [ Right Thu ]
            , interval = 2
            , dtStart = Time.millisToPosix 1549555200000
            , tzid = TimeZone.america__chicago ()
        }
    , window =
        { start = Time.millisToPosix 1550886858009
        , end = Time.millisToPosix 1644198858009
        }
    , dates = [ 1551974400000, 1553180400000, 1554390000000, 1555599600000, 1556809200000, 1558018800000, 1559228400000, 1560438000000, 1561647600000, 1562857200000, 1564066800000, 1565276400000, 1566486000000, 1567695600000, 1568905200000, 1570114800000, 1571324400000, 1572534000000, 1573747200000, 1574956800000, 1576166400000, 1577376000000 ]
    }


example6 : Example
example6 =
    { description = "First Friday of every quarter"
    , rrule =
        [ "RRULE:FREQ=MONTHLY;INTERVAL=3;BYDAY=1FR"
        , "DTSTART;TZID=America/Denver:20210101T100000"
        ]
    , recurrence =
        { defaultRules
            | frequency = Monthly
            , byDay = [ Left ( 1, Fri ) ]
            , interval = 3
            , dtStart = Time.millisToPosix 1609520400000
        }
    , window =
        { start = (Time.millisToPosix 1617606023000 {- 2021-04-05T07:00:23.000Z -})
        , end = (Time.millisToPosix 1672938023000 {- "2023-01-05T17:00:23.000Z" -})
        }
    , dates = [ 1625241600000, 1633104000000, 1641574800000, 1648828800000, 1656691200000, 1665158400000 ]
    }


example7 : Example
example7 =
    { description = "Every third year on the 1st, 100th, and 200th day"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970101T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=3;BYYEARDAY=1,100,200;UNTIL=20060102T170023Z"
        ]
    , recurrence =
        { defaultRules
            | frequency = Yearly
            , dtStart = Time.millisToPosix 852127200000
            , untilCount = Just (Until <| Time.millisToPosix 1136221223000)
            , interval = 3
            , byYearDay = [ 1, 100, 200 ]
            , tzid = TimeZone.america__new_york ()
        }
    , window =
        { start = (Time.millisToPosix 949770023000 {- 2000-02-05T17:00:23.000Z -})
        , end = (Time.millisToPosix 1644198858009 {- 2022-02-07T01:54:18.009Z -})
        }
    , dates = [ 955285200000, 963925200000, 1041429600000, 1049979600000, 1058619600000, 1136124000000 ]
    }


{-| betweenWindow.start is right before second intervalWindow.start
-}
example8_1 : Example
example8_1 =
    { description = "Every other year 9am EST on the 10th of January, February, and March"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970310T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1,2,3;"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 858002400000
            , interval = 2
            , byMonth = [ 1, 2, 3 ]
            , tzid = TimeZone.america__new_york ()
        }
    , window =
        { start = (Time.millisToPosix 915166799000 {- 1 second before new-years day 1999 -})
        , end = (Time.millisToPosix 1047304700000 {- 2003-03-10T01:58:20.000Z -})
        }
    , dates = [ 915976800000, 918655200000, 921074400000, 979135200000, 981813600000, 984232800000, 1042207200000, 1044885600000 ]
    }


{-| betweenWindow.start is after the second intervalWindow.start but right before the first instance
-}
example8_2 : Example
example8_2 =
    { description = "Every other year 9am EST on the 10th of January, February, and March (1st altered betweenWindow)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970310T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1,2,3;"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 858002400000
            , interval = 2
            , byMonth = [ 1, 2, 3 ]
            , tzid = TimeZone.america__new_york ()
        }
    , window =
        { start = (Time.millisToPosix 915976700000 {- 2000-02-05T17:00:23.000Z -})
        , end = (Time.millisToPosix 1047304700000 {- 2003-03-10T01:58:20.000Z -})
        }
    , dates = [ 915976800000, 918655200000, 921074400000, 979135200000, 981813600000, 984232800000, 1042207200000, 1044885600000 ]
    }


{-| betweenWindow.start is after the second intervalWindow.start but after after the window's last instance
betweenWindow.end is right before the fourth windows first instance
-}
example8_3 : Example
example8_3 =
    { description = "Every other year 9am EST on the 10th of January, February, and March (2nd altered betweenWindow)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19970310T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1,2,3;"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 858002400000
            , interval = 2
            , byMonth = [ 1, 2, 3 ]
            , tzid = TimeZone.america__new_york ()
        }
    , window =
        { start = (Time.millisToPosix 921074400001 {- 1999-03-10T14:00:00.001Z -})
        , end = (Time.millisToPosix 1042207199999 {- 2003-01-10T13:59:59.999Z -})
        }
    , dates = [ 979135200000, 981813600000, 984232800000 ]
    }


example9 : Example
example9 =
    { description = "Every U.S. Presidential Election between Nov 2004 (a millisecond after the election) and 2024 (a millisecond before the election)"
    , rrule =
        [ "DTSTART;TZID=America/New_York:19961105T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 847202400000
            , interval = 4
            , byMonth = [ 11 ]
            , byDay = [ Right Tue ]
            , byMonthDay = [ 2, 3, 4, 5, 6, 7, 8 ]
            , tzid = TimeZone.america__new_york ()
        }
    , window =
        { start = (Time.millisToPosix 1099404000001 {- 2004-11-02T14:00:00.001Z -})
        , end = (Time.millisToPosix 1730815199999 {- 2024-11-05T13:59:59.999Z -})
        }
    , dates = [ 1225807200000, 1352210400000, 1478613600000, 1604412000000 ]
    }


example10 : Example
example10 =
    { description = "window.start is an off-day of an every-fifth-day event"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20201215T090000"
        , "RRULE:FREQ=DAILY;INTERVAL=5"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 1608048000000
            , frequency = Daily
            , interval = 5
        }
    , window =
        { start = (Time.millisToPosix 1608220800000 {- 2020-12-17T16:00:00.000Z -})
        , end = (Time.millisToPosix 1613577600000 {- 2021-02-17T16:00:00.000Z -})
        }
    , dates = [ 1608480000000, 1608912000000, 1609344000000, 1609776000000, 1610208000000, 1610640000000, 1611072000000, 1611504000000, 1611936000000, 1612368000000, 1612800000000, 1613232000000 ]
    }


example11 : Example
example11 =
    { description = "window.start is an off-week of a tuesday-every-third-week event"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20201215T090000"
        , "RRULE:FREQ=WEEKLY;INTERVAL=3"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 1608048000000
            , frequency = Weekly
            , interval = 3
        }
    , window =
        { start = (Time.millisToPosix 1608566400000 {- 2020-12-21T16:00:00.000Z -})
        , end = (Time.millisToPosix 1615132800000 {- 2021-03-07T16:00:00.000Z -})
        }
    , dates = [ 1609862400000, 1611676800000, 1613491200000 ]
    }


example12 : Example
example12 =
    { description = "window.start is an off-month of an every-other-month event"
    , rrule =
        [ "DTSTART;TZID=America/Chicago:20180703T170000"
        , "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=1TU"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 1530655200000
            , frequency = Monthly
            , interval = 2
            , byDay = [ Left ( 1, Tue ) ]
            , tzid = TimeZone.america__chicago ()
        }
    , window =
        { start = (Time.millisToPosix 1550886858009 {- 2019-02-23T01:54:18.009Z -})
        , end = (Time.millisToPosix 1644198858009 {- 2022-02-07T01:54:18.009Z -})
        }
    , dates = [ 1551826800000, 1557266400000, 1562104800000, 1567548000000, 1572994800000, 1578438000000, 1583276400000, 1588716000000, 1594159200000, 1598997600000, 1604444400000, 1609887600000, 1614726000000, 1620165600000, 1625608800000, 1631052000000, 1635890400000, 1641337200000 ]
    }


example13 : Example
example13 =
    { description = "window.start in an off-year of an every-ten-year event"
    , rrule =
        [ "DTSTART;TZID=America/Denver:20201215T090000"
        , "RRULE:FREQ=YEARLY;INTERVAL=10"
        ]
    , recurrence =
        { defaultRules
            | dtStart = Time.millisToPosix 1608048000000
            , frequency = Yearly
            , interval = 10
        }
    , window =
        { start = (Time.millisToPosix 1609804800000 {- 2021-01-05T00:00:00.000Z -})
        , end = (Time.millisToPosix 1923609600000 {- 2030-12-16T00:00:00.000Z -})
        }
    , dates = [ 1923580800000 ]
    }
