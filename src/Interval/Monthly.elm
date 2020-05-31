module Interval.Monthly exposing (..)

import Date
import Either exposing (Either(..))
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra as TE exposing (Interval(..))
import Util exposing (Window)



-- Window Helpers


checker =
    { isExcluded = isExcluded
    , isIncluded = isIncluded
    }


{-| MONTHLY is limited when BYMONTH is defined
-}
isExcluded : Recurrence -> Posix -> Bool
isExcluded rrule time =
    List.member
        (Time.toMonth rrule.tzid time |> Date.monthToNumber)
        rrule.byMonth


{-| MONTHLY is expanded when BYDAY, and BYMONTHDAY is defined

NOTE: BYDAY limits if BYMONTHDAY is present, otherwise it expands.

-}
isIncluded : Recurrence -> Posix -> Bool
isIncluded rrule time =
    case ( List.isEmpty rrule.byMonthDay, List.isEmpty rrule.byDay ) of
        ( True, False ) ->
            List.any (isByDay rrule.tzid time) rrule.byDay

        ( False, True ) ->
            List.any (isByMonthDay rrule.tzid time) rrule.byMonthDay

        ( False, False ) ->
            List.any (isByMonthDay rrule.tzid time) rrule.byMonthDay
                && (not <| List.any (isByDay rrule.tzid time) rrule.byDay)

        ( True, True ) ->
            False


{-| Each BYDAY value can also be preceded by a positive (+n) or
negative (-n) integer. If present, this indicates the nth
occurrence of a specific day within the MONTHLY or YEARLY "RRULE".

For example, within a MONTHLY rule, +1MO (or simply 1MO)
represents the first Monday within the month, whereas -1MO
represents the last Monday of the month. The numeric value in a
BYDAY rule part with the FREQ rule part set to YEARLY corresponds
to an offset within the month when the BYMONTH rule part is
present, and corresponds to an offset within the year when the
BYWEEKNO or BYMONTH rule parts are present. If an integer
modifier is not present, it means all days of this type within the
specified frequency. For example, within a MONTHLY rule, MO
represents all Mondays within the month. The BYDAY rule part MUST
NOT be specified with a numeric value when the FREQ rule part is
not set to MONTHLY or YEARLY. Furthermore, the BYDAY rule part
MUST NOT be specified with a numeric value with the FREQ rule part
set to YEARLY when the BYWEEKNO rule part is specified.

-}
isByMonthDay : Zone -> Posix -> Int -> Bool
isByMonthDay zone time byMonthDay =
    if byMonthDay > 0 then
        Time.toDay zone time == byMonthDay

    else
        -- TODO
        True


isByDay : Zone -> Posix -> Either ( Int, Weekday ) Weekday -> Bool
isByDay zone time byday =
    case byday of
        Right weekday ->
            Time.toWeekday zone time == weekday

        Left ordinality ->
            onOrdinalDay zone time ordinality


onOrdinalDay : Zone -> Posix -> ( Int, Weekday ) -> Bool
onOrdinalDay zone time ( ordinal, weekday ) =
    -- TODO is 0 cardinality valid?
    if Time.toWeekday zone time /= weekday then
        False

    else if ordinal > 5 || ordinal < -5 || ordinal == 0 then
        False

    else if ordinal > 0 then
        TE.floor TE.Month zone time
            |> TE.ceiling (Util.weekdayToInterval weekday) zone
            |> ordinalHelp zone Th (Time.toDay zone time) (abs ordinal - 1)

    else
        TE.ceiling TE.Month zone time
            |> TE.add TE.Day -1 zone
            |> TE.floor (Util.weekdayToInterval weekday) zone
            |> ordinalHelp zone ThToLast (Time.toDay zone time) (abs ordinal - 1)


type Ordinality
    = Th
    | ThToLast


ordinalHelp : Zone -> Ordinality -> Int -> Int -> Posix -> Bool
ordinalHelp zone ordinality originalMonthDay counter current =
    let
        ( adder, comparator ) =
            case ordinality of
                Th ->
                    ( 1, Util.lt )

                ThToLast ->
                    ( -1, Util.gt )

        next =
            TE.add TE.Week adder zone current
    in
    if comparator next current then
        -- Did we cross into the next month?
        False

    else if counter == 0 then
        originalMonthDay == Time.toDay zone current

    else
        ordinalHelp zone ordinality originalMonthDay (counter - 1) next
