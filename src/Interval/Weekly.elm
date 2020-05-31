module Interval.Weekly exposing (..)

import Date
import Either exposing (Either(..))
import Generator
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)


generate : Recurrence -> List Posix
generate rrule =
    -- TODO Shouldn't assume dtStart is a valid instance time.
    -- Need to validate first and find the first valid instance time if necessary.
    Generator.run rrule (withinByRules rrule)


withinByRules : Recurrence -> Posix -> Bool
withinByRules rrule current =
    checkExpands rrule current && checkLimits rrule current


{-| WEEKLY is limited when BYMONTH is defined
-}
checkLimits : Recurrence -> Posix -> Bool
checkLimits rrule time =
    List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth
        |> not


{-| WEEKLY is expanded when BYDAY is defined
-}
checkExpands : Recurrence -> Posix -> Bool
checkExpands rrule time =
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
