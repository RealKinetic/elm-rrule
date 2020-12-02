module Examples.Custom exposing (..)

import Either exposing (Either(..))
import Recurrence exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Weekday(..))
import TimeZone


defaultRules : Recurrence
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
