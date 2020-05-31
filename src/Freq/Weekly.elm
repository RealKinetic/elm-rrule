module Freq.Weekly exposing (checker)

import By
import Recurrence exposing (Recurrence)
import Time exposing (Posix)


checker =
    { isExcluded = isExcluded
    , isIncluded = isIncluded
    }


{-| WEEKLY is limited when BYMONTH is defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    not <| By.month rrule time


{-| WEEKLY is expanded when BYDAY is defined
-}
isIncluded : Recurrence -> Posix -> Bool
isIncluded rrule time =
    By.day rrule time
