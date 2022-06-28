module RRule exposing
    ( Error(..)
    , Frequency(..)
    , RRule
    , UntilCount(..)
    , all
    , between
    , errorToString
    , fromStrings
    , fromStringsWithStart
    , ianaTimezones
    , toText
    )

import Date exposing (Date, Month)
import Dict
import Either exposing (Either(..))
import Ordinal
import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Problem(..), Step(..))
import Set
import String.Extra
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as TE exposing (Interval(..))
import TimeZone
import Util


{-| -}
type alias RRule =
    { frequency : Frequency
    , weekStart : Weekday
    , interval : Int
    , dtStart : Posix

    -- TODO Support floating times by having the user provide a fallback IANA / Zone?
    , tzid : Zone
    , untilCount : Maybe UntilCount
    , byDay : List (Either ( Int, Weekday ) Weekday)
    , byMonthDay : List Int
    , byMonth : List Int
    , byWeekNo : List Int
    , byYearDay : List Int
    , exdates : List Posix
    , rdates : List Posix
    }


type Frequency
    = Daily
    | Weekly
    | Monthly
    | Yearly


type UntilCount
    = Count Int
      -- TODO Support Date and DateTime?
    | Until Posix



-- GENERATOR


between : { start : Posix, end : Posix } -> RRule -> List Posix
between ({ start, end } as betweenWindow) preNormalizedRRule =
    let
        rrule =
            normalizeRRule preNormalizedRRule

        check =
            computeCheck rrule

        firstInstance =
            firstValidInstance rrule check

        ceiling =
            if Util.lt end Util.year2250 then
                end

            else
                Util.year2250

        runHelp { firstInstanceTime, startingWindow } =
            run (canOptimizeRun rrule)
                check
                ceiling
                rrule
                startingWindow
                firstInstanceTime
                []
    in
    if hasCount rrule then
        {- We just naively run any RRULE with a COUNT and filter
           out everything outside the betweenWindow. This shouldn't
           be too troublesome since most event's with COUNTs have a
           relatively small COUNT.
        -}
        runHelp
            { firstInstanceTime = firstInstance
            , startingWindow = initWindow firstInstance rrule
            }
            |> List.filter (\time -> Util.gte time start)

    else
        betweenHelper rrule check firstInstance betweenWindow
            |> Maybe.map runHelp
            |> Maybe.withDefault []


all : RRule -> List Posix
all preNormalizedRRule =
    let
        rrule =
            normalizeRRule preNormalizedRRule

        check =
            computeCheck rrule

        firstInstance =
            firstValidInstance rrule check
    in
    run (canOptimizeRun rrule)
        check
        Util.year2250
        rrule
        (initWindow firstInstance rrule)
        firstInstance
        []


toText : RRule -> String
toText =
    normalizeRRule
        >> toTextHelp
        >> List.filter (String.isEmpty >> not)
        >> String.join " "



-- Internal


firstValidInstance : RRule -> Check -> Posix
firstValidInstance rrule check =
    let
        helper t =
            if withinRuleset rrule check t then
                t

            else
                helper (TE.add TE.Day 1 rrule.tzid t)
    in
    if rrule.interval /= 1 then
        {- TODO Very hard to determine the first valid instance if interval > 1
           In this case we just have to assume the dtStart is a valid instance.
           We might think about adding decoding validation to catch this issue.

              e.g., In the rrrule below, is the _current_ month the valid
              "interval window" or is it the next month where our first valid
              instance will be found?

              RRULE:FREQ=MONTHLY,BYDAY=2FR,INTERVAL=2 (2nd Friday every other month)
              Where our DTSTART is _after_ the 2nd Friday of the month.
        -}
        rrule.dtStart

    else
        helper rrule.dtStart


run : Bool -> Check -> Posix -> RRule -> Window -> Posix -> List Posix -> List Posix
run optimizedMode check timeCeiling rrule window current acc =
    let
        nextByDay =
            TE.add TE.Day 1 rrule.tzid current

        nextTime =
            if optimizedMode then
                -- The vast majority of these will be YEARLY events, e.g. birthdays.
                TE.add (freqToInterval rrule.frequency)
                    rrule.interval
                    rrule.tzid
                    current

            else if inWindow nextByDay window then
                -- TODO check to see if this behaving correctly by adding tests
                -- for rrule's with RULE:FREQ=WEEKLY;BYDAY=SA,SU,MO;WEEKSTART=SU and MO;
                nextByDay

            else
                bumpToNextWindow rrule current

        nextWindow =
            if inWindow nextTime window then
                window

            else
                computeNextWindow rrule window

        withoutExDates =
            List.filter
                (\time ->
                    List.map Time.posixToMillis rrule.exdates
                        |> Set.fromList
                        |> Set.member (Time.posixToMillis time)
                        |> not
                )
    in
    if pastUntilCount timeCeiling rrule.untilCount current acc then
        {-
           TODO EXDATES should not affect the generation of instances vis-a-vis COUNT,
            But can the same be said for RDATE? I suspect RDATE behaves similiarly to EXDATE.
        -}
        Util.dedupeAndSortTimes (acc ++ rrule.rdates)
            |> withoutExDates

    else if current |> withinRuleset rrule check then
        {- Note: DO NOT partially apply `run` in the `let` statement above.
           We need to keep the full call to `run` down here for tail-call optimization.
        -}
        run optimizedMode check timeCeiling rrule nextWindow nextTime (current :: acc)

    else
        run optimizedMode check timeCeiling rrule nextWindow nextTime acc


{-| Rounds betweenWindow.start up to the nearest valid recurring instance time.
-}
betweenHelper :
    RRule
    -> Check
    -> Posix
    -> { start : Posix, end : Posix }
    -> Maybe { firstInstanceTime : Posix, startingWindow : Window }
betweenHelper rrule check firstInstance { start, end } =
    let
        mergeWithDTSTART =
            Util.mergeTimeOf rrule.tzid firstInstance

        mergedStartTime_ =
            mergeWithDTSTART start

        mergedStartTime =
            if Util.gt start mergedStartTime_ then
                {- Bump it up to the next day to ensure we're not
                   lowering the betweenWindow.start floor. If we did
                   this, we'd risk including instance times which lay
                   outside of the window.

                -}
                TE.add TE.Day 1 rrule.tzid mergedStartTime_

            else
                mergedStartTime_

        findFirstInstance window time =
            {- Short circuit in case we exceed the betweenWindow.end -}
            if Util.gt time end then
                Nothing

            else if
                Util.gte time start
                    && (time |> withinRuleset rrule check)
                    && inWindow time window
            then
                {- Return time if it's greater than betweenWindow.start,
                   a valid instance, and in the current window
                -}
                Just { firstInstanceTime = time, startingWindow = window }

            else if not (inWindow time window) then
                {- Move to the next window if we're outside the window bounds -}
                let
                    nextWindow =
                        computeNextWindow rrule window
                in
                findFirstInstance nextWindow
                    (mergeWithDTSTART nextWindow.lowerBound)

            else
                {- Otherwise move up to the next day. -}
                findFirstInstance window (TE.add TE.Day 1 rrule.tzid time)
    in
    if Util.gt start end then
        -- Prevent neverending instance search if start > end
        Nothing

    else if Util.gte firstInstance start then
        Just
            { firstInstanceTime = firstInstance
            , startingWindow = initWindow firstInstance rrule
            }

    else
        findFirstInstance (initWindow firstInstance rrule) mergedStartTime


{-| Information not contained in the rule necessary to determine the
various recurrence instance start time and dates are derived from
the DTSTART.

If the BYDAY, BYMONTHDAY, or BYMONTH rule part are missing,
the appropriate day or month are retrieved from the DTSTART property.

For example, "FREQ=YEARLY;BYMONTH=1" doesn't specify a specific day
within the month or a time. This information would be the same
as what is specified for DTSTART.

-}
normalizeRRule : RRule -> RRule
normalizeRRule rrule =
    case rrule.frequency of
        Weekly ->
            case rrule.byDay of
                [] ->
                    { rrule
                        | byDay = [ Right <| Time.toWeekday rrule.tzid rrule.dtStart ]
                    }

                _ ->
                    rrule

        Monthly ->
            case ( rrule.byDay, rrule.byMonthDay ) of
                ( [], [] ) ->
                    { rrule
                        | byMonthDay = [ Time.toDay rrule.tzid rrule.dtStart ]
                    }

                _ ->
                    rrule

        Yearly ->
            case
                ( ( rrule.byDay, rrule.byMonthDay )
                , ( rrule.byYearDay, rrule.byWeekNo, rrule.byMonth )
                )
            of
                ( ( [], [] ), ( [], [], [] ) ) ->
                    -- with no BYRULES we infer the month and monthday
                    { rrule
                        | byMonth = [ Date.monthToNumber <| Time.toMonth rrule.tzid rrule.dtStart ]
                        , byMonthDay = [ Time.toDay rrule.tzid rrule.dtStart ]
                    }

                ( ( [], [] ), ( [], [], _ :: _ ) ) ->
                    -- with only byMonth we infer the monthday
                    { rrule
                        | byMonthDay = [ Time.toDay rrule.tzid rrule.dtStart ]
                    }

                ( ( _ :: _, [] ), ( [], [], [] ) ) ->
                    -- with only byDay we infer the month
                    { rrule
                        | byMonth = [ Date.monthToNumber <| Time.toMonth rrule.tzid rrule.dtStart ]
                    }

                ( ( [], [] ), ( [], _ :: _, [] ) ) ->
                    -- with only byWeekNo we infer the weekday
                    { rrule
                        | byDay = [ Right <| Time.toWeekday rrule.tzid rrule.dtStart ]
                    }

                _ ->
                    rrule

        Daily ->
            rrule


hasCount : RRule -> Bool
hasCount rrule =
    case rrule.untilCount of
        Just (Count _) ->
            True

        _ ->
            False


{-| TODO Support rulesets
-}
withinRuleset : RRule -> Check -> Posix -> Bool
withinRuleset =
    withinByRules



-- BYRRULE Helpers


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


withinByRules : RRule -> Check -> Posix -> Bool
withinByRules rrule { limits, expands } time =
    let
        apply : ByRule -> Bool
        apply f =
            f rrule time
    in
    (not <| List.any (apply >> not) limits)
        && List.all apply expands


computeCheck : RRule -> Check
computeCheck rrule =
    case rrule.frequency of
        Daily ->
            { limits = [ day, monthDay, month ]
            , expands = []
            }

        Weekly ->
            { limits = [ month ]
            , expands = [ day ]
            }

        Monthly ->
            -- See 'Note 1' above
            case ( Util.notEmpty rrule.byMonthDay, Util.notEmpty rrule.byDay ) of
                ( False, True ) ->
                    { limits = [ month ]
                    , expands = [ day ]
                    }

                ( True, False ) ->
                    { limits = [ month ]
                    , expands = [ monthDay ]
                    }

                ( True, True ) ->
                    { limits = [ day, month ]
                    , expands = [ monthDay ]
                    }

                ( False, False ) ->
                    { limits = [ month ]
                    , expands = []
                    }

        Yearly ->
            -- See 'Note 2' above
            case
                ( Util.notEmpty rrule.byDay
                , Util.notEmpty rrule.byYearDay || Util.notEmpty rrule.byMonthDay
                )
            of
                ( False, _ ) ->
                    { limits = []
                    , expands = [ month, weekNo, yearDay, monthDay ]
                    }

                ( True, True ) ->
                    { limits = [ day ]
                    , expands = [ month, weekNo, yearDay, monthDay ]
                    }

                ( True, False ) ->
                    case ( Util.notEmpty rrule.byWeekNo, Util.notEmpty rrule.byMonth ) of
                        ( True, _ ) ->
                            { limits = []
                            , expands = [ month, weekNo |> and day ]
                            }

                        ( _, True ) ->
                            { limits = []
                            , expands = [ month |> and day ]
                            }

                        _ ->
                            { limits = []
                            , expands = [ day ]
                            }


and : ByRule -> ByRule -> ByRule
and by1 by2 =
    \rrule_ time_ ->
        by1 rrule_ time_ && by2 rrule_ time_


{-| This essentially checks if a given rrule has only one expanding BYRULE.

A "non-optimized run" involves stepping up day-by-day and checking if the given
day fits within the BYRULES. If an RRule has only one expanding BYRULE we can
safely optimize, that is step up by the RRule's frequency rather than day-by-day.
The greatly optimizes many weekly and monthly, and most yearly recurring events.
e.g. Birthdays, holidays, meeting every 15th of the month.

Note: This function must be handed a normalized rrule for optimization to work.

-}
canOptimizeRun : RRule -> Bool
canOptimizeRun rrule =
    case rrule.frequency of
        Daily ->
            True

        Weekly ->
            case rrule.byDay of
                [ _ ] ->
                    True

                _ ->
                    False

        Monthly ->
            case rrule.byMonthDay of
                [ md ] ->
                    {- Not every month has days 29, 30, or 31.
                       Also, excludes negative numbers since e.g., 3rd to last
                       day of the month is not always the same monthday.
                    -}
                    md >= 1 && md <= 28

                _ ->
                    {- In other cases we have to check day by day.
                       e.g., If it's BYDAY=3FR, we can't add a month and assume
                       we're on the third friday.
                    -}
                    False

        Yearly ->
            case ( rrule.byMonth, rrule.byMonthDay ) of
                {- We use Time.Extra.add in our stepwise optimization process.
                   Time.Extra adds years by month/monthday not yearday.

                   e.g., 1 year + 20200320 == 20210320

                   The yearday of a given month&monthday will differ based on
                   whether it's a leap year.
                -}
                ( [ m ], [ md ] ) ->
                    (md >= 1
                     {- exclude negative numbers.
                        nth to last day of month will change if it's a leap year.
                     -}
                    )
                        && (not <| m == 2 && md == 29 {- Exlude leap years -})
                        && List.isEmpty rrule.byYearDay
                        && List.isEmpty rrule.byWeekNo
                        && (List.isEmpty rrule.byDay
                            {- We know byDay is an expand since byMonthDay is present.
                               See 'Note 2' above
                            -}
                           )

                _ ->
                    False



-- BYRULES


type alias ByRule =
    RRule -> Posix -> Bool


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


dayHelp : RRule -> Posix -> Either ( Int, Weekday ) Weekday -> Bool
dayHelp rrule time byday =
    case byday of
        Right weekday_ ->
            Time.toWeekday rrule.tzid time == weekday_

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
onOrdinalDayMonthly zone time ( ordinal, weekday_ ) =
    if Time.toWeekday zone time /= weekday_ then
        False

    else if ordinal > 5 || ordinal < -5 || ordinal == 0 then
        False

    else if ordinal > 0 then
        TE.floor TE.Month zone time
            |> TE.ceiling (Util.weekdayToInterval weekday_) zone
            |> ordinalDayMonthlyHelp zone Th (Time.toDay zone time) (abs ordinal - 1)

    else
        TE.ceiling TE.Month zone time
            |> TE.add TE.Day -1 zone
            |> TE.floor (Util.weekdayToInterval weekday_) zone
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
onOrdinalDayYearly zone time ( ordinal, weekday_ ) =
    let
        jan1 =
            TE.floor TE.Year zone time

        thOrdinalDay =
            jan1
                |> TE.floor (Util.weekdayToInterval weekday_) zone
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
weekNoHelp zone weekStart_ time weekNoNum =
    let
        date =
            Date.fromPosix zone time

        thWeekNo =
            Util.weekNumber weekStart_ date

        thToLastWeekNo =
            if Util.is53WeekYear weekStart_ (Time.toYear zone time) then
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
trueIfEmpty list_ bool =
    if List.isEmpty list_ then
        True

    else
        bool



--------------------------------------


{-| -}
pastUntilCount : Posix -> Maybe UntilCount -> Posix -> List Posix -> Bool
pastUntilCount timeCeiling mUntilCount current times =
    pastUntilCount_ mUntilCount current times || Util.gte current timeCeiling


pastUntilCount_ : Maybe UntilCount -> Posix -> List Posix -> Bool
pastUntilCount_ mUntilCount current times =
    case mUntilCount of
        Just (Count count_) ->
            List.length times >= count_ || List.length times >= 5000

        Just (Until until_) ->
            -- UNTIL is inclusive
            Time.posixToMillis current > Time.posixToMillis until_

        Nothing ->
            False


{-| -}
freqToInterval : Frequency -> TE.Interval
freqToInterval freq =
    case freq of
        Daily ->
            TE.Day

        Weekly ->
            TE.Week

        Monthly ->
            TE.Month

        Yearly ->
            TE.Year



----------------------------------------------------------


{-| The window in which events can occur for any given Interval

Take for instance RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR
Mon, Wed, Fri every other week.

The window will be a week long, but will skip a week when the next window
is computed.

Put in other terms, the window exists to help us when rrule.interval > 1.

For example:

Let's say we have an event which occurs every three weeks on Monday.
FREQ=WEEKLY;INTERVAL=3;BYDAY=MO

[SMTWTFS] SMTWTFS SMTWTFS [SMTWTFS] SMTWTFS

The M's within the brackets are Mondays that sit in an "on-week", or a window.
Essentially, the window helps us skip over "off-weeks".
The same is true for other frequencies. We skip off-days, off-months, and off-years.

-}
type alias Window =
    { lowerBound : Posix, upperBound : Posix }


initWindow : Posix -> RRule -> Window
initWindow lowerBound rrule =
    { lowerBound = lowerBound
    , upperBound =
        TE.ceiling (windowInterval rrule) rrule.tzid lowerBound
            |> Util.subtract 1
    }


windowInterval : RRule -> TE.Interval
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


{-| -}
inWindow : Posix -> Window -> Bool
inWindow time { lowerBound, upperBound } =
    let
        ( time_, lowerBound_, upperBound_ ) =
            ( Time.posixToMillis time
            , Time.posixToMillis lowerBound
            , Time.posixToMillis upperBound
            )
    in
    time_ >= lowerBound_ && time_ <= upperBound_


{-| -}
computeNextWindow : RRule -> Window -> Window
computeNextWindow rrule window =
    let
        timeUnit =
            freqToInterval rrule.frequency

        newUpperBound =
            -- Have to add 1 since our previous window upper bound is 23:59:59.999
            window.upperBound
                |> Util.add 1
                |> TE.add timeUnit rrule.interval rrule.tzid

        newLowerBound =
            newUpperBound
                |> TE.add timeUnit -1 rrule.tzid
    in
    { lowerBound = newLowerBound
    , upperBound = newUpperBound |> Util.subtract 1
    }


{-| -}
bumpToNextWindow : RRule -> Posix -> Posix
bumpToNextWindow rrule time =
    let
        next =
            TE.add (freqToInterval rrule.frequency)
                rrule.interval
                rrule.tzid
                time
                |> TE.floor (freqToInterval rrule.frequency) rrule.tzid
    in
    Util.mergeTimeOf rrule.tzid time next



-----------------------------------------------------------------------
-- DECODERS
-----------------------------------------------------------------------
{-
   TODO - All day recurring events do not have a timezone... How do I handle this.
-}


fromStrings : List String -> Result Error RRule
fromStrings rrules =
    getDTSTART
        |> andThen (\{ start, zone } -> decoder zone start)
        |> decodeLines rrules


fromStringsWithStart : List String -> Zone -> Posix -> Result Error RRule
fromStringsWithStart rrules zone start =
    decoder zone start
        |> decodeLines rrules


type alias RawRRules =
    { dtStart : DTSTART
    , rrule_ : RRULE
    , exdate_ : EXDATE
    , rdate_ : RDATE
    }


decoder : Zone -> Posix -> Decoder RRule
decoder zone start =
    succeed RawRRules
        |> andMap (succeed <| DTSTART zone start)
        |> andMap getRRULE
        |> andMap getEXDATE
        |> andMap getRDATE
        |> andThen (rawRRulesToRecurrence zone)


rawRRulesToRecurrence : Zone -> RawRRules -> Decoder RRule
rawRRulesToRecurrence zone { dtStart, rrule_, exdate_, rdate_ } =
    succeed RRule
        |> andMap (frequency rrule_)
        |> andMap (weekStart rrule_)
        |> andMap (interval rrule_)
        |> andMap (succeed dtStart.start)
        |> andMap (succeed dtStart.zone)
        |> andMap (untilCount zone rrule_)
        |> andMap (byDay rrule_)
        |> andMap (byMonthDay rrule_)
        |> andMap (byMonth rrule_)
        |> andMap (byWeekNo rrule_)
        |> andMap (byYearDay rrule_)
        |> andMap (exdates zone exdate_)
        |> andMap (succeed [])


{-| "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20200120T065959Z;BYDAY=MO,TU,WE,TH"
-}
type RRULE
    = RRULE String


getRRULE : Decoder RRULE
getRRULE =
    Decoder <| find True RRULE "RRULE"


frequency : RRULE -> Decoder Frequency
frequency (RRULE rrule) =
    P.succeed identity
        |. chompThrough "FREQ"
        |. P.symbol "="
        |= (P.chompUntilEndOr ";"
                |> P.getChompedString
                |> P.andThen parseFreq
           )
        |> runParserOn rrule


{-| Page 42/43 iCal Spec <https://tools.ietf.org/html/rfc5545#section-3.3.10>

    The WKST rule part specifies the day on which the workweek starts.
    Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
    significant when a WEEKLY "RRULE" has an interval greater than 1,
    and a BYDAY rule part is specified.  This is also significant when
    in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
    default value is MO.

-}
weekStart : RRULE -> Decoder Weekday
weekStart (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "WKST"
            |. P.symbol "="
            |= weekday
            |> runParserOn rrule
        , succeed Mon
        ]


byDay : RRULE -> Decoder (List (Either ( Int, Weekday ) Weekday))
byDay (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYDAY"
            |. P.symbol "="
            |= parseWeekdays
            |> runParserOn rrule
        , succeed []
        ]


parseWeekdays : Parser (List (Either ( Int, Weekday ) Weekday))
parseWeekdays =
    P.oneOf
        [ weekday
            |> P.map Right
            |> P.backtrackable
        , P.succeed Tuple.pair
            |= parseNegatableInt
            |= weekday
            |> P.map Left
        ]
        |> list


byMonthDay : RRULE -> Decoder (List Int)
byMonthDay (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYMONTHDAY"
            |. P.symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


byMonth : RRULE -> Decoder (List Int)
byMonth (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYMONTH"
            |. P.symbol "="
            |= list P.int
            |> runParserOn rrule
        , succeed []
        ]


byWeekNo : RRULE -> Decoder (List Int)
byWeekNo (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYWEEKNO"
            |. P.symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


byYearDay : RRULE -> Decoder (List Int)
byYearDay (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYYEARDAY"
            |. P.symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


untilCount : Zone -> RRULE -> Decoder (Maybe UntilCount)
untilCount zone rrule =
    oneOf [ until zone rrule, count rrule ]
        |> maybe


{-| UNTIL=19971224T000000Z
-}
until : Zone -> RRULE -> Decoder UntilCount
until zone (RRULE rrule) =
    P.succeed identity
        |. chompThrough "UNTIL"
        |. P.symbol "="
        {- TODO this can be DATE as well as DATETIME pg 41 of the spec
           The UNTIL rule part defines a DATE or DATE-TIME value that bounds
           the recurrence rule in an inclusive manner.

           TODO Should UNTIL DATE's be cast into posix w/ UTC or TZID
        -}
        |= P.oneOf
            [ parseDateTime
                |> P.map (TE.partsToPosix Time.utc >> Until)
                |> P.backtrackable
            , parseDateToParts
                |> P.map (TE.partsToPosix zone >> Until)
            ]
        |> runParserOn rrule


{-| COUNT=42
-}
count : RRULE -> Decoder UntilCount
count (RRULE rrule) =
    P.succeed identity
        |. chompThrough "COUNT"
        |. P.symbol "="
        |= P.int
        |> P.map Count
        |> runParserOn rrule


{-| INTERVAL=2
-}
interval : RRULE -> Decoder Int
interval (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "INTERVAL"
            |. P.symbol "="
            |= P.int
            |> runParserOn rrule
        , succeed 1
        ]


parseFreq : String -> Parser Frequency
parseFreq str =
    case str of
        "DAILY" ->
            P.succeed Daily

        "WEEKLY" ->
            P.succeed Weekly

        "MONTHLY" ->
            P.succeed Monthly

        "YEARLY" ->
            P.succeed Yearly

        _ ->
            P.problem ("Unknown FREQ: " ++ str)


weekday : Parser Weekday
weekday =
    chompChars 2
        |> P.andThen
            (\str ->
                case str of
                    "MO" ->
                        P.succeed Mon

                    "TU" ->
                        P.succeed Tue

                    "WE" ->
                        P.succeed Wed

                    "TH" ->
                        P.succeed Thu

                    "FR" ->
                        P.succeed Fri

                    "SA" ->
                        P.succeed Sat

                    "SU" ->
                        P.succeed Sun

                    _ ->
                        P.problem ("Unknown FREQ: " ++ str)
            )


{-| "20190806T055959"
-}
parseDateTime : Parser TE.Parts
parseDateTime =
    P.succeed TE.Parts
        |= chompDigits 4
        |= (chompDigits 2 |> P.map Date.numberToMonth)
        |= chompDigits 2
        |. P.symbol "T"
        |= chompDigits 2
        |= chompDigits 2
        |= chompDigits 2
        |= P.succeed 0


{-| "20190806"
-}
parseDateToParts : Parser TE.Parts
parseDateToParts =
    P.succeed TE.Parts
        |= chompDigits 4
        |= (chompDigits 2 |> P.map Date.numberToMonth)
        |= chompDigits 2
        |= P.succeed 0
        |= P.succeed 0
        |= P.succeed 0
        |= P.succeed 0


parseNegatableInt : Parser Int
parseNegatableInt =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.int
        , P.int
        ]


{-| DTSTART;TZID=America/Denver:20190603T090000;
-}
type alias DTSTART =
    { zone : Zone
    , start : Posix
    }


getDTSTART : Decoder DTSTART
getDTSTART =
    find True identity "DTSTART"
        |> Decoder
        |> andThen tzidAndDtStart


{-| RDATE;
-}
type RDATE
    = RDATE String


getRDATE : Decoder RDATE
getRDATE =
    Decoder <| find False RDATE "RDATE"


tzidAndDtStart : String -> Decoder DTSTART
tzidAndDtStart dtstartString =
    -- TODO Default to UTC if TZID doesn't exist?
    P.succeed Tuple.pair
        |. chompThrough "TZID"
        |. P.symbol "="
        |= (P.chompUntil ":"
                |> P.getChompedString
                |> P.andThen parseTzid
           )
        |. P.symbol ":"
        -- TODO this can be DATE as well as DATETIME pg 41 of the spec
        |= P.oneOf
            [ parseDateTime |> P.backtrackable
            , parseDateToParts
            ]
        |> P.map
            (\( zone, parts ) ->
                { zone = zone
                , start = TE.partsToPosix zone parts
                }
            )
        |> runParserOn dtstartString


ianaTimezones : Dict.Dict String (() -> Zone)
ianaTimezones =
    TimeZone.zones
        |> Dict.insert "UTC" (\() -> Time.utc)
        |> Dict.insert "Etc/GMT" (\() -> Time.utc)
        |> Dict.insert "Etc/UTC" (\() -> Time.utc)
        |> Dict.insert "PST8PDT" TimeZone.america__los_angeles
        |> Dict.insert "Asia/Calcutta" TimeZone.asia__kolkata
        |> Dict.insert "America/Indianapolis" TimeZone.america__indiana__indianapolis


parseTzid : String -> Parser Zone
parseTzid ianaZoneName =
    case Dict.get ianaZoneName ianaTimezones of
        Just zone ->
            P.succeed <| zone ()

        Nothing ->
            P.problem <| "Unknown IANA zone: " ++ ianaZoneName


{-| "EXDATE;TZID=America/Denver:20170419T083000,20190717T090000,20190718T090000,20190730T090000"
-}
type EXDATE
    = EXDATE String


getEXDATE : Decoder EXDATE
getEXDATE =
    Decoder <| find False EXDATE "EXDATE"


exdates : Zone -> EXDATE -> Decoder (List Posix)
exdates zone (EXDATE exdateString) =
    -- TODO Default to UTC if TZID doesn't exist?
    P.oneOf
        [ ifEmptyString []
        , P.succeed Tuple.pair
            |. chompThrough "TZID"
            |. P.symbol "="
            |= (P.chompUntil ":"
                    |> P.getChompedString
                    |> P.andThen parseTzid
               )
            |. P.symbol ":"
            |= list parseDateTime
            |> P.map
                (\( exdateZone, exdates_ ) ->
                    List.map (TE.partsToPosix exdateZone) exdates_
                )
        , P.succeed identity
            |. chompThrough "VALUE"
            |. P.symbol "="
            |. chompThrough "DATE"
            |. P.symbol ":"
            |= list parseDateToParts
            |> P.map (List.map (TE.partsToPosix zone))
        ]
        |> runParserOn exdateString


{-| TODO Should we just returning a (Maybe EXDATE) rather than (EXDATE "")?
-}
ifEmptyString : a -> Parser a
ifEmptyString fallback =
    P.end |> P.map (always fallback)



{- List Helper -}


list : Parser a -> Parser (List a)
list chompingParser =
    P.loop [] (listHelper chompingParser)
        |> P.map List.reverse


listHelper : Parser a -> List a -> Parser (Step (List a) (List a))
listHelper chompingParser items =
    P.succeed (\item step -> step (item :: items))
        |= chompingParser
        |= P.oneOf
            [ P.succeed Loop |. P.symbol ","
            , P.succeed Done |. P.symbol ";"
            , P.succeed Done |. P.end
            ]



{- Helper -}


{-| Support multiple RRULE, EXDATES, RDATES in a recurrence list?

EXRULES...? Ha. No.

-}
find : Bool -> (String -> a) -> String -> List String -> Result Error a
find required tagger name vals =
    case vals of
        [] ->
            if required then
                Err <| NotFound name

            else
                Ok <| tagger ""

        x :: xs ->
            if String.startsWith name x then
                Ok <| tagger x

            else
                find required tagger name xs


{-| Decoder

See <https://github.com/zwilias/elm-json-in-elm/blob/master/src/Json/Decoder.elm>
for nearly identical patterns.

-}
type Decoder a
    = Decoder (List String -> Result Error a)


type Error
    = NotFound String
    | ParseFailure String
    | OneOf (List Error)


errorToString : Error -> String
errorToString error =
    case error of
        NotFound info ->
            "Not Found: " ++ info

        ParseFailure info ->
            "RRULE Parse Failure: " ++ info

        OneOf errors ->
            List.map errorToString errors
                |> String.join "\n\n"


decodeLines : List String -> Decoder a -> Result Error a
decodeLines lines (Decoder decoderF) =
    decoderF (List.map String.trim lines)


succeed : a -> Decoder a
succeed val =
    Decoder (\_ -> Ok val)


map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder decoderFA) (Decoder decoderFB) =
    Decoder <|
        \val ->
            Result.map2 f (decoderFA val) (decoderFB val)


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toB (Decoder decoderF) =
    Decoder <|
        \val ->
            case decoderF val of
                Ok decoded ->
                    decodeLines val (toB decoded)

                Err err ->
                    Err err


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    map2 (|>)


maybe : Decoder a -> Decoder (Maybe a)
maybe (Decoder decoderF) =
    Decoder <|
        \val ->
            case decoderF val of
                Ok a ->
                    Ok <| Just a

                Err _ ->
                    Ok Nothing


oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    let
        initialResult : ( List Error, Maybe a )
        initialResult =
            ( [], Nothing )

        combineResults : List String -> Decoder a -> ( List Error, Maybe a ) -> ( List Error, Maybe a )
        combineResults val (Decoder decoderF) ( errors, result ) =
            case result of
                Just _ ->
                    ( errors, result )

                Nothing ->
                    case decoderF val of
                        Ok val_ ->
                            ( errors, Just val_ )

                        Err e ->
                            ( e :: errors, Nothing )

        wrapUp : ( List Error, Maybe a ) -> Result Error a
        wrapUp ( errors, result ) =
            Maybe.map Ok result
                |> Maybe.withDefault (Err <| OneOf <| List.reverse errors)
    in
    Decoder <|
        \val ->
            List.foldl (combineResults val) initialResult decoders
                |> wrapUp



{- Parsing Helpers -}


{-| -}
chompThrough : String -> Parser ()
chompThrough str =
    P.chompUntil str
        |. P.keyword str


runParserOn : String -> Parser a -> Decoder a
runParserOn val parser =
    Decoder <|
        \_ ->
            P.run parser val
                |> Result.mapError (deadEndsToString >> ParseFailure)


stringToInt : String -> Parser Int
stringToInt str =
    case String.toInt str of
        Just num ->
            P.succeed num

        Nothing ->
            P.problem (str ++ " is not an int")


chompDigits : Int -> Parser Int
chompDigits length =
    chompChars length
        |> P.andThen stringToInt


chompChars : Int -> Parser String
chompChars length =
    P.getChompedString <|
        P.loop 0 (chompCharsHelper length)


chompCharsHelper : Int -> Int -> Parser (Step Int Int)
chompCharsHelper length count_ =
    if length == count_ then
        P.succeed ()
            |> P.map (\_ -> Done count_)

    else
        P.succeed (Loop (count_ + 1))
            |. P.chompIf (\_ -> True)



-- Error Message Help


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    List.foldl (++) "" (List.map deadEndToString deadEnds)


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
    let
        position : String
        position =
            "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
    in
    case deadEnd.problem of
        Expecting str ->
            "Expecting " ++ str ++ " at " ++ position

        ExpectingInt ->
            "ExpectingInt at " ++ position

        ExpectingHex ->
            "ExpectingHex at " ++ position

        ExpectingOctal ->
            "ExpectingOctal at " ++ position

        ExpectingBinary ->
            "ExpectingBinary at " ++ position

        ExpectingFloat ->
            "ExpectingFloat at " ++ position

        ExpectingNumber ->
            "ExpectingNumber at " ++ position

        ExpectingVariable ->
            "ExpectingVariable at " ++ position

        ExpectingSymbol str ->
            "ExpectingSymbol " ++ str ++ " at " ++ position

        ExpectingKeyword str ->
            "ExpectingKeyword " ++ str ++ " at " ++ position

        ExpectingEnd ->
            "ExpectingEnd at " ++ position

        UnexpectedChar ->
            "UnexpectedChar at " ++ position

        Problem str ->
            "ProblemString " ++ str ++ " at " ++ position

        BadRepeat ->
            "BadRepeat at " ++ position



-- toText
{- Text examples

   ['Every day', 'RRULE:FREQ=DAILY'],
   ['Every week', 'RRULE:FREQ=WEEKLY'],
   ['Every week on Tuesday', 'RRULE:FREQ=WEEKLY;BYDAY=TU'],
   ['Every week on Monday, Wednesday', 'RRULE:FREQ=WEEKLY;BYDAY=MO,WE'],
   ['Every weekday', 'RRULE:FREQ=WEEKLY;BYDAY=MO,TU,WE,TH,FR'],
   ['Every 2 weeks', 'RRULE:INTERVAL=2;FREQ=WEEKLY'],
   ['Every month', 'RRULE:FREQ=MONTHLY'],
   ['Every 6 months', 'RRULE:INTERVAL=6;FREQ=MONTHLY'],
   ['Every year', 'RRULE:FREQ=YEARLY'],
   ['Every year on the 1st Friday', 'RRULE:FREQ=YEARLY;BYDAY=+1FR'],
   ['Every year on the 13th Friday', 'RRULE:FREQ=YEARLY;BYDAY=+13FR'],
   ['Every month on the 4th', 'RRULE:FREQ=MONTHLY;BYMONTHDAY=4'],
   ['Every month on the 4th last', 'RRULE:FREQ=MONTHLY;BYMONTHDAY=-4'],
   ['Every month on the 3rd Tuesday', 'RRULE:FREQ=MONTHLY;BYDAY=+3TU'],
   ['Every month on the 3rd last Tuesday', 'RRULE:FREQ=MONTHLY;BYDAY=-3TU'],
   ['Every month on the last Monday', 'RRULE:FREQ=MONTHLY;BYDAY=-1MO'],
   ['Every month on the 2nd last Friday', 'RRULE:FREQ=MONTHLY;BYDAY=-2FR'],
   ['Every week until January 1, 2007', 'RRULE:FREQ=WEEKLY;UNTIL=20070101T080000Z'],
   ['Every week for 20 times', 'RRULE:FREQ=WEEKLY;COUNT=20'],

-}
-- TODO bymonthday
-- TODO byyearday


toTextHelp : RRule -> List String
toTextHelp rrule =
    let
        untilCountText =
            untilCountToText rrule

        startHelp adjectiveUnit nounUnit =
            case rrule.interval of
                1 ->
                    adjectiveUnit

                2 ->
                    "Every other " ++ nounUnit

                _ ->
                    "Every " ++ String.fromInt rrule.interval ++ " " ++ nounUnit ++ "s"
    in
    case rrule.frequency of
        Daily ->
            [ startHelp "Daily" "day"
            , untilCountText
            ]

        Weekly ->
            [ startHelp "Weekly" "week"
            , weeklyByDayText rrule.byDay
            , untilCountText
            ]

        Monthly ->
            [ startHelp "Monthly" "month"
            , monthlyByDayText rrule.byDay
            , byMonthText rrule.byMonthDay
            , untilCountText
            ]

        Yearly ->
            [ startHelp "Yearly" "year"
            , untilCountText
            ]


weeklyByDayText : List (Either ( Int, Weekday ) Weekday) -> String
weeklyByDayText byDay_ =
    let
        -- 'Weekly' doesn't include ordinal BYDAY rules
        days =
            List.filterMap
                (\eitherDay ->
                    case eitherDay of
                        Left _ ->
                            Nothing

                        Right wd ->
                            Just wd
                )
                byDay_
    in
    case days of
        [] ->
            ""

        [ day_ ] ->
            "on " ++ weekdayToTextLong day_

        -- Use short weekday text if there is more than one weekday to save space.
        days_ ->
            "on " ++ (String.Extra.toSentenceOxford <| List.map weekdayToTextShort days_)


monthlyByDayText : List (Either ( Int, Weekday ) Weekday) -> String
monthlyByDayText byDay_ =
    let
        {- TODO change the prepending text (e.g., "on the ") depending on the
           the types of byday
           e.g. These are not right:
           "Every 2 months on the Tueday for 20 months"
           "Every month on the Saturday on the 7th, 8th, 9th, 10th, 11th, 12th, and 13th for 10 months"
           "Every month on the Friday on the 13th for 10 months"
        -}
        textParts =
            List.filterMap
                (\day_ ->
                    case day_ of
                        Left ( ordinal, weekday_ ) ->
                            if ordinal > 0 then
                                Just (Ordinal.ordinal ordinal ++ " " ++ weekdayToTextLong weekday_)

                            else if ordinal == -1 then
                                Just ("last " ++ weekdayToTextLong weekday_)

                            else
                                Just (Ordinal.ordinal (abs ordinal) ++ " last " ++ weekdayToTextLong weekday_)

                        Right wd ->
                            Just <| weekdayToTextLong wd
                )
                byDay_
    in
    textParts
        |> String.Extra.toSentenceOxford
        |> (++) (if_ (List.isEmpty textParts) "" "on the ")


byMonthText : List Int -> String
byMonthText monthDays_ =
    monthDays_
        |> List.map
            (\monthDay_ ->
                if monthDay_ > 0 then
                    Ordinal.ordinal monthDay_

                else if monthDay_ == -1 then
                    "last day"

                else
                    Ordinal.ordinal (abs monthDay_) ++ " to last"
            )
        |> String.Extra.toSentenceOxford
        |> (++) (if_ (List.isEmpty monthDays_) "" "on the ")


weekdayToTextLong : Weekday -> String
weekdayToTextLong weekday_ =
    case weekday_ of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


weekdayToTextShort : Weekday -> String
weekdayToTextShort weekday_ =
    case weekday_ of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


untilCountToText : RRule -> String
untilCountToText rrule =
    case rrule.untilCount of
        Just (Count count_) ->
            "(" ++ String.fromInt count_ ++ if_ (count_ == 1) " time)" " times)"

        Just (Until until_) ->
            "until " ++ Date.format "MMMM ddd, y" (Date.fromPosix rrule.tzid until_)

        Nothing ->
            ""


{-| Ternary helper
-}
if_ : Bool -> a -> a -> a
if_ cond a b =
    if cond then
        a

    else
        b
