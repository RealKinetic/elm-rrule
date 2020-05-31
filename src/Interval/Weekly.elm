module Interval.Weekly exposing (checker)

import Date
import Either exposing (Either(..))
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)


checker =
    { isExcluded = isExcluded
    , isIncluded = isIncluded
    }


{-| WEEKLY is limited when BYMONTH is defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth


{-| WEEKLY is expanded when BYDAY is defined
-}
isIncluded : Recurrence -> Posix -> Bool
isIncluded rrule time =
    if List.isEmpty rrule.byDay then
        True

    else
        withoutCardinalByDays rrule.byDay
            |> List.member (Time.toWeekday rrule.tzid time)


withoutCardinalByDays : List (Either ( Int, Weekday ) Weekday) -> List Weekday
withoutCardinalByDays =
    List.filterMap
        (\day ->
            case day of
                Right weekday ->
                    Just weekday

                _ ->
                    Nothing
        )
