module Generator exposing (..)

import RRule exposing (Frequency(..), Recurrence)
import Time exposing (Posix)
import Time.Extra as TE
import Util exposing (Window)


run : Recurrence -> (Posix -> Bool) -> List Posix
run rrule withinByRules =
    -- TODO Shouldn't assume dtStart is a valid instance time.
    -- Need to validate first and find the first valid instance time if necessary.
    runHelp rrule (initWindow rrule) withinByRules rrule.dtStart []


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


runHelp : Recurrence -> Window -> (Posix -> Bool) -> Posix -> List Posix -> List Posix
runHelp rrule window withinByRules current acc =
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

        runNext =
            runHelp rrule nextWindow withinByRules nextTime
    in
    if Util.pastUntilCount rrule.untilCount current acc then
        List.reverse acc

    else if withinByRules current then
        runNext (current :: acc)

    else
        runNext acc


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
        RRule.Daily ->
            True

        RRule.Weekly ->
            List.isEmpty rrule.byDay

        RRule.Monthly ->
            List.isEmpty rrule.byMonthDay && List.isEmpty rrule.byDay

        RRule.Yearly ->
            False
