module Sandbox exposing (..)

import Either exposing (Either(..))
import RRule exposing (Frequency(..), UntilCount(..))
import Time exposing (Weekday(..))
import TimeZone


rrulesExample =
    [ "EXDATE;TZID=America/Denver:20170419T083000,20190717T090000,20190718T090000,20190730T090000"
    , "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20190806T055959Z;BYDAY=MO,TU,WE,TH"
    , "DTSTART;TZID=America/Denver:20190603T090000;"
    ]
        |> String.join "\n"


rruleExample =
    "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20190806T055959Z;BYDAY=MO,TU,WE,TH"


denver : Time.Zone
denver =
    TimeZone.america__denver ()


{-| "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20190806T055959Z;BYDAY=MO,TU,WE,TH"
-}
testRule =
    { weekStart = Mon
    , frequency = Weekly
    , interval = 1

    -- Monday, June 3, 2019 9:00:00 AM GMT-06:00 DST
    , dtStart = Time.millisToPosix 1559574000000
    , tzid = TimeZone.america__denver ()
    , untilCount = Just (Count 20)

    --, untilCount = Just (Until <| Time.millisToPosix 3137460700000) --  3137460700000)
    , byDay = [ Right Mon, Right Tue, Right Wed, Right Thu ]
    , byWeekNo = []
    , byMonthDay = []
    , byMonth = []
    , exdates = Nothing
    }
