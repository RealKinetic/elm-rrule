module Generator exposing (..)

import Freq.Daily as Daily
import Freq.Monthly as Monthly
import Freq.Weekly as Weekly
import RRule exposing (Frequency(..), Recurrence)
import Time exposing (Posix)
import Time.Extra as TE
import Util exposing (Window)


run : Recurrence -> List Posix
run rrule =
    -- TODO Shouldn't assume dtStart is a valid instance time.
    -- Need to validate first and find the first valid instance time if necessary.
    runHelp rrule (initWindow rrule) rrule.dtStart []


runHelp : Recurrence -> Window -> Posix -> List Posix -> List Posix
runHelp rrule window current acc =
    let
        nextByDay =
            -- This is not as performant as it could be.
            -- We are checking every day within a given window.
            -- This means we'll check 365 days to find a single yearly event.
            TE.add TE.Day 1 rrule.tzid current

        nextTime =
            if rrule |> hasNoExpands then
                TE.add (Util.freqToInterval rrule.frequency)
                    rrule.interval
                    rrule.tzid
                    current

            else if Util.inWindow nextByDay window then
                -- TODO check to see if this behaving correctly by adding tests
                -- for rrule's with RULE:FREQ=WEEEKLY;BYDAY=SA,SU,MO;WEEKSTART=SU and MO;
                nextByDay

            else
                Util.bumpToNextWindow rrule current

        nextWindow =
            if Util.inWindow nextTime window then
                window

            else
                Util.computeNextWindow rrule window
    in
    if Util.pastUntilCount rrule.untilCount current acc then
        List.reverse acc

    else if current |> withinByRules rrule then
        runHelp rrule nextWindow nextTime (current :: acc)

    else
        runHelp rrule nextWindow nextTime acc



-- WINDOW


initWindow : Recurrence -> Window
initWindow rrule =
    { lowerBound = rrule.dtStart
    , upperBound =
        TE.ceiling (windowInterval rrule) rrule.tzid rrule.dtStart
            |> Util.subtract 1
    }


windowInterval : Recurrence -> TE.Interval
windowInterval rrule =
    case rrule.frequency of
        Daily ->
            TE.Day

        Weekly ->
            Util.weekdayToInterval rrule.weekStart

        Monthly ->
            TE.Month

        Yearly ->
            TE.Year


{-|

        +----------+--------+--------+-------+-------+------+-------+------+
        |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYMONTH   |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYDAY     |Limit   |Limit   |Limit  |Limit  |Expand|Note 1 |Note 2|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|
        +----------+--------+--------+-------+-------+------+-------+------+
        |BYSETPOS  |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Limit |
        +----------+--------+--------+-------+-------+------+-------+------+

      Note 1:  Limit if BYMONTHDAY is present; otherwise, special expand
               for MONTHLY.

      Note 2:  Limit if BYYEARDAY or BYMONTHDAY is present; otherwise,
               special expand for WEEKLY if BYWEEKNO present; otherwise,
               special expand for MONTHLY if BYMONTH present; otherwise,
               special expand for YEARLY.

-}
hasNoExpands : Recurrence -> Bool
hasNoExpands rrule =
    case rrule.frequency of
        Daily ->
            True

        Weekly ->
            List.isEmpty rrule.byDay

        Monthly ->
            List.isEmpty rrule.byMonthDay
                && List.isEmpty rrule.byDay

        Yearly ->
            False



-- BYxx RULES


type alias Check =
    { isExcluded : Recurrence -> Posix -> Bool
    , isIncluded : Recurrence -> Posix -> Bool
    }


withinByRules : Recurrence -> Posix -> Bool
withinByRules rrule time =
    let
        check { isIncluded, isExcluded } =
            isIncluded rrule time && (not <| isExcluded rrule time)
    in
    case rrule.frequency of
        Daily ->
            check Daily.checker

        Weekly ->
            check Weekly.checker

        Monthly ->
            check Monthly.checker

        Yearly ->
            -- TODO
            True
