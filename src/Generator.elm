module Generator exposing (..)

import By exposing (ByRule)
import Recurrence exposing (Frequency(..), Recurrence)
import Time exposing (Posix)
import Time.Extra as TE
import Util exposing (Window, notEmpty)


run : Recurrence -> List Posix
run preNormalizedRRule =
    let
        rrule =
            Recurrence.normalize preNormalizedRRule
    in
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



-- BYxx RULES


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
type alias Check =
    { limits : List ByRule
    , expands : List ByRule
    }


withinByRules : Recurrence -> Posix -> Bool
withinByRules rrule time =
    let
        apply : ByRule -> Bool
        apply f =
            f rrule time

        check : Check -> Bool
        check { limits, expands } =
            (not <| List.any (apply >> not) limits)
                && List.all apply expands
    in
    check <| withinByRulesHelp rrule


withinByRulesHelp : Recurrence -> Check
withinByRulesHelp rrule =
    case rrule.frequency of
        Daily ->
            { limits = [ By.day, By.monthDay, By.month ]
            , expands = []
            }

        Weekly ->
            { limits = [ By.month ]
            , expands = [ By.day ]
            }

        Monthly ->
            -- See 'Note 1' above
            case ( notEmpty rrule.byMonthDay, notEmpty rrule.byDay ) of
                ( False, True ) ->
                    { limits = [ By.month ]
                    , expands = [ By.day ]
                    }

                ( True, False ) ->
                    { limits = [ By.month ]
                    , expands = [ By.monthDay ]
                    }

                ( True, True ) ->
                    { limits = [ By.day, By.month ]
                    , expands = [ By.monthDay ]
                    }

                ( False, False ) ->
                    { limits = [ By.month ]
                    , expands = []
                    }

        Yearly ->
            -- See 'Note 2' above
            case ( notEmpty rrule.byDay, notEmpty rrule.byYearDay || notEmpty rrule.byMonthDay ) of
                ( False, _ ) ->
                    { limits = []
                    , expands = [ By.month, By.weekNo, By.yearDay, By.monthDay ]
                    }

                ( True, True ) ->
                    { limits = [ By.day ]
                    , expands = [ By.month, By.weekNo, By.yearDay, By.monthDay ]
                    }

                ( True, False ) ->
                    case ( notEmpty rrule.byWeekNo, notEmpty rrule.byMonth ) of
                        ( True, _ ) ->
                            { limits = []
                            , expands = [ By.month, By.weekNo |> and By.day ]
                            }

                        ( _, True ) ->
                            { limits = []
                            , expands = [ By.month |> and By.day ]
                            }

                        _ ->
                            { limits = []
                            , expands = [ By.day ]
                            }


and : ByRule -> ByRule -> ByRule
and by1 by2 =
    \rrule_ time_ ->
        by1 rrule_ time_ && by2 rrule_ time_


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
