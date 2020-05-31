module By exposing (day, month, monthDay)

import Date
import Either exposing (Either(..))
import Recurrence exposing (Recurrence)
import Time exposing (Posix, Weekday, Zone)
import Time.Extra as TE
import Util


{-| BYDAY

Each BYDAY value can also be preceded by a positive (+n) or
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
day : Recurrence -> Posix -> Bool
day rrule time =
    List.any (dayHelp rrule.tzid time) rrule.byDay
        |> trueIfEmpty rrule.byDay


dayHelp : Zone -> Posix -> Either ( Int, Weekday ) Weekday -> Bool
dayHelp zone time byday =
    case byday of
        Right weekday ->
            Time.toWeekday zone time == weekday

        Left ordinality ->
            onOrdinalDay zone time ordinality


onOrdinalDay : Zone -> Posix -> ( Int, Weekday ) -> Bool
onOrdinalDay zone time ( ordinal, weekday ) =
    if Time.toWeekday zone time /= weekday then
        False

    else if ordinal > 5 || ordinal < -5 || ordinal == 0 then
        -- TODO This can actually go from 1 to 53 due to YEARLY
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


{-| BYMONTHDAY
-}
monthDay : Recurrence -> Posix -> Bool
monthDay rrule time =
    List.any (monthDayHelp rrule.tzid time) rrule.byMonthDay
        |> trueIfEmpty rrule.byMonthDay


monthDayHelp : Zone -> Posix -> Int -> Bool
monthDayHelp zone time byMonthDay =
    if byMonthDay > 0 then
        Time.toDay zone time == byMonthDay

    else
        Time.toDay zone time
            - Util.daysInMonth (Time.toYear zone time) (Time.toMonth zone time)
            |> (==) (byMonthDay + 1)


{-| BYMONTH
-}
month : Recurrence -> Posix -> Bool
month rrule time =
    List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth
        |> trueIfEmpty rrule.byMonth


{-| TODO Change BYxx RULES to Maybes?
-}
trueIfEmpty : List a -> Bool -> Bool
trueIfEmpty list bool =
    if List.isEmpty list then
        True

    else
        bool
