module Examples.Between exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), RRule, UntilCount(..))
import Time exposing (Posix, Weekday(..))
import TimeZone


type alias Example =
    { description : String
    , rrules : List String
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
    , rrules =
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
    , rrules =
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
    , rrules =
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
    , rrules =
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
