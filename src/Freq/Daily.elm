module Freq.Daily exposing (..)

import Date
import Either exposing (Either(..))
import RRule exposing (Recurrence)
import Time exposing (Posix, Weekday, Zone)
import Util


checker =
    { isExcluded = isExcluded
    , isIncluded = \_ _ -> True
    }


{-| MONTHLY is limited when BYMONTH, BYDAY, or BYMONTHDAY are defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    [ List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth
        |> unlessEmpty rrule.byMonth
        |> not
    , isByDay rrule time
        |> unlessEmpty rrule.byDay
        |> not
    , List.any (isByMonthDay rrule.tzid time) rrule.byMonthDay
    ]
        |> List.any identity


unlessEmpty : List a -> Bool -> Bool
unlessEmpty list bool =
    if List.isEmpty list then
        True

    else
        bool


isByMonthDay : Zone -> Posix -> Int -> Bool
isByMonthDay zone time byMonthDay =
    if byMonthDay > 0 then
        Time.toDay zone time == byMonthDay

    else
        Time.toDay zone time
            - Util.daysInMonth (Time.toYear zone time) (Time.toMonth zone time)
            |> (==) (byMonthDay + 1)


isByDay : Recurrence -> Posix -> Bool
isByDay rrule time =
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
