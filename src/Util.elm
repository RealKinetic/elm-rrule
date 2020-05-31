module Util exposing (..)

import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra as TE exposing (Interval(..))


freqToInterval : Frequency -> TE.Interval
freqToInterval freq =
    case freq of
        Daily ->
            Day

        Weekly ->
            Week

        Monthly ->
            Month

        Yearly ->
            Year


weekdayToInterval : Weekday -> TE.Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Monday

        Tue ->
            Tuesday

        Wed ->
            Wednesday

        Thu ->
            Thursday

        Fri ->
            Friday

        Sat ->
            Saturday

        Sun ->
            Sunday


gte : Posix -> Posix -> Bool
gte t1 t2 =
    Time.posixToMillis t1 >= Time.posixToMillis t2


gt : Posix -> Posix -> Bool
gt t1 t2 =
    Time.posixToMillis t1 > Time.posixToMillis t2


lt : Posix -> Posix -> Bool
lt t1 t2 =
    Time.posixToMillis t1 < Time.posixToMillis t2


{-| Not DST safe
-}
subtract : Int -> Posix -> Posix
subtract int time =
    (Time.posixToMillis time - int)
        |> Time.millisToPosix


{-| Not DST safe
-}
add : Int -> Posix -> Posix
add int time =
    (Time.posixToMillis time + int)
        |> Time.millisToPosix


pastUntilCount : Maybe UntilCount -> Posix -> List Posix -> Bool
pastUntilCount mUntilCount current times =
    case mUntilCount of
        Just (Count count) ->
            List.length times >= count || List.length times >= 1000

        Just (Until until) ->
            -- TODO is UNTIL inclusive or exclusive?
            -- I think it's inclusive, based on gcals 23:59:59 setting
            Time.posixToMillis current > Time.posixToMillis until

        Nothing ->
            False


{-| The window in which events can occur for any given Interval

Take for instance RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR
Mon, Wed, Fri every other week.

The window will be a week long, but will skip a week when the next window
is computed.

-}
type alias Window =
    { lowerBound : Posix, upperBound : Posix }


inWindow : Posix -> Window -> Bool
inWindow time { lowerBound, upperBound } =
    let
        ( time_, lowerBound_, upperBound_ ) =
            ( Time.posixToMillis time
            , Time.posixToMillis lowerBound
            , Time.posixToMillis upperBound
            )
    in
    -- TODO Inclusive or exclusive?
    -- Depends on how we compute the bounds.
    time_ >= lowerBound_ && time_ <= upperBound_


computeNextWindow : Recurrence -> Window -> Window
computeNextWindow rrule window =
    let
        timeUnit =
            freqToInterval rrule.frequency

        newUpperBound =
            -- Have to add 1 since our previous window is 23:59:59.000
            window.upperBound
                |> add 1
                |> TE.add timeUnit rrule.interval rrule.tzid
                |> subtract 1

        newLowerBound =
            newUpperBound
                |> TE.add timeUnit -1 rrule.tzid
    in
    { lowerBound = newLowerBound
    , upperBound = newUpperBound
    }


bumpToNextWindow : Recurrence -> Posix -> Posix
bumpToNextWindow rrule time =
    let
        timeParts =
            time |> TE.posixToParts rrule.tzid

        nextParts =
            TE.add (freqToInterval rrule.frequency)
                rrule.interval
                rrule.tzid
                time
                |> TE.floor (freqToInterval rrule.frequency) rrule.tzid
                |> TE.posixToParts rrule.tzid
    in
    { timeParts
        | year = nextParts.year
        , month = nextParts.month
        , day = nextParts.day
    }
        |> TE.partsToPosix rrule.tzid
