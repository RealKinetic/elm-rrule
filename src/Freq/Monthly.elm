module Freq.Monthly exposing (..)

import By
import Recurrence exposing (Recurrence)
import Time exposing (Posix)


checker =
    { isExcluded = isExcluded
    , isIncluded = isIncluded
    }


{-| MONTHLY is limited when BYMONTH is defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    not <| By.month rrule time


{-| MONTHLY is expanded when BYDAY, and BYMONTHDAY is defined

NOTE: BYDAY limits if BYMONTHDAY is also defined, otherwise BYDAY expands.

-}
isIncluded : Recurrence -> Posix -> Bool
isIncluded rrule time =
    case ( List.isEmpty rrule.byMonthDay, List.isEmpty rrule.byDay ) of
        ( True, False ) ->
            By.day rrule time

        ( False, True ) ->
            By.monthDay rrule time

        ( False, False ) ->
            By.monthDay rrule time && By.day rrule time

        ( True, True ) ->
            False
