module Freq.Daily exposing (..)

import By
import Recurrence exposing (Recurrence)
import Time exposing (Posix)


checker =
    { isExcluded = isExcluded
    , isIncluded = isIncluded
    }


{-| MONTHLY is limited when BYMONTH, BYDAY, or BYMONTHDAY are defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    [ not <| By.month rrule time
    , not <| By.day rrule time
    , not <| By.monthDay rrule time
    ]
        |> List.any identity


isIncluded _ _ =
    True
