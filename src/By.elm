module By exposing (ByRule, day, month, monthDay, weekNo, yearDay)

import Date
import Either exposing (Either(..))
import Recurrence exposing (Frequency(..), Recurrence)
import Time exposing (Posix, Weekday, Zone)
import Time.Extra as TE
import Util


type alias ByRule =
    Recurrence -> Posix -> Bool


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
day : ByRule
day rrule time =
    List.any (dayHelp rrule time) rrule.byDay
        |> trueIfEmpty rrule.byDay


dayHelp : Recurrence -> Posix -> Either ( Int, Weekday ) Weekday -> Bool
dayHelp rrule time byday =
    case byday of
        Right weekday ->
            Time.toWeekday rrule.tzid time == weekday

        Left ordinality ->
            case rrule.frequency of
                Monthly ->
                    onOrdinalDayMonthly rrule.tzid time ordinality

                Yearly ->
                    onOrdinalDayYearly rrule.tzid time ordinality

                _ ->
                    False


type Ordinality
    = Th
    | ThToLast


onOrdinalDayMonthly : Zone -> Posix -> ( Int, Weekday ) -> Bool
onOrdinalDayMonthly zone time ( ordinal, weekday ) =
    if Time.toWeekday zone time /= weekday then
        False

    else if ordinal > 5 || ordinal < -5 || ordinal == 0 then
        False

    else if ordinal > 0 then
        TE.floor TE.Month zone time
            |> TE.ceiling (Util.weekdayToInterval weekday) zone
            |> ordinalDayMonthlyHelp zone Th (Time.toDay zone time) (abs ordinal - 1)

    else
        TE.ceiling TE.Month zone time
            |> TE.add TE.Day -1 zone
            |> TE.floor (Util.weekdayToInterval weekday) zone
            |> ordinalDayMonthlyHelp zone ThToLast (Time.toDay zone time) (abs ordinal - 1)


ordinalDayMonthlyHelp : Zone -> Ordinality -> Int -> Int -> Posix -> Bool
ordinalDayMonthlyHelp zone ordinality originalMonthDay counter current =
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
        ordinalDayMonthlyHelp zone ordinality originalMonthDay (counter - 1) next


onOrdinalDayYearly : Zone -> Posix -> ( Int, Weekday ) -> Bool
onOrdinalDayYearly zone time ( ordinal, weekday ) =
    let
        jan1 =
            TE.floor TE.Year zone time

        thOrdinalDay =
            jan1
                |> TE.floor (Util.weekdayToInterval weekday) zone
                |> (\t ->
                        if jan1 == t then
                            t

                        else
                            TE.add TE.Week 1 zone t
                   )
                |> Util.mergeTimeOf zone time
                |> TE.add TE.Week (ordinal - 1) zone
    in
    if ordinal > 0 then
        thOrdinalDay == time
        --(Util.weekNumber weekStart (Date.fromPosix zone time) == (ordinal + 1))
        --    && (Time.toWeekday zone time == weekday)

    else
        -- TODO thToLast for neg numbers
        False


{-| BYMONTHDAY
-}
monthDay : ByRule
monthDay rrule time =
    let
        thMonthDay =
            Time.toDay rrule.tzid time

        thToLastMonthDay =
            (thMonthDay - 1)
                - Util.daysInMonth
                    (Time.toYear rrule.tzid time)
                    (Time.toMonth rrule.tzid time)

        match monthDayNum =
            thMonthDay == monthDayNum || thToLastMonthDay == monthDayNum
    in
    List.any match rrule.byMonthDay
        |> trueIfEmpty rrule.byMonthDay


{-| BYMONTH
-}
month : ByRule
month rrule time =
    List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth
        |> trueIfEmpty rrule.byMonth


{-| BYWEEKNO
-}
weekNo : ByRule
weekNo rrule time =
    List.any (weekNoHelp rrule.tzid rrule.weekStart time) rrule.byWeekNo
        |> trueIfEmpty rrule.byWeekNo


weekNoHelp : Zone -> Weekday -> Posix -> Int -> Bool
weekNoHelp zone weekStart time weekNoNum =
    let
        date =
            Date.fromPosix zone time

        thWeekNo =
            Util.weekNumber weekStart date

        thToLastWeekNo =
            if Util.is53WeekYear weekStart (Time.toYear zone time) then
                (thWeekNo - 1) - 53

            else
                (thWeekNo - 1) - 52
    in
    thWeekNo == weekNoNum || thToLastWeekNo == weekNoNum


{-| BYYEARDAY
-}
yearDay : ByRule
yearDay rrule time =
    let
        year =
            Time.toYear rrule.tzid time

        thYearDay =
            Time.toDay rrule.tzid time
                + Util.daysBeforeMonth year (Time.toMonth rrule.tzid time)

        thToLastYearDay =
            (thYearDay - 1) - Util.daysInYear year

        match yearDayNum =
            thYearDay == yearDayNum || thToLastYearDay == yearDayNum
    in
    List.any match rrule.byYearDay
        |> trueIfEmpty rrule.byYearDay



-- Utils


trueIfEmpty : List a -> Bool -> Bool
trueIfEmpty list bool =
    if List.isEmpty list then
        True

    else
        bool
