module Interval.Weekly exposing (..)

import Date
import Either exposing (Either(..))
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra exposing (Interval(..))
import Util


generate : Recurrence -> List Posix
generate rrule =
    -- TODO Shouldn't assume dtStart is a valid instance time.
    -- Need to validate first and find the first valid instance time if necessary.
    generateHelp rrule (computeWindow rrule rrule.dtStart) rrule.dtStart []


generateHelp : Recurrence -> Window -> Posix -> List Posix -> List Posix
generateHelp rrule window current acc =
    let
        nextByDay =
            -- This is not as performant as it could be.
            -- We are checking every day within a given window.
            -- This means we'll check 365 days to find a single yearly event.
            Time.Extra.add Time.Extra.Day 1 rrule.tzid current

        next =
            if List.isEmpty rrule.byDay then
                Time.Extra.add Time.Extra.Week rrule.interval rrule.tzid current

            else if inWindow nextByDay window then
                -- TODO check to see if this behaving correctly by adding tests
                -- for rrule's with RULE:FREQ=WEEEKLY;BYDAY=SA,SU,MO;WEEKSTART=SU and MO;
                nextByDay

            else
                bumpToNextWindow rrule current

        newWindow =
            if inWindow next window then
                window

            else
                computeNextWindow rrule window
    in
    if pastUntilCount rrule.untilCount current acc then
        List.reverse acc

    else if checkGTE rrule current && checkLT rrule current then
        generateHelp rrule newWindow next (current :: acc)

    else
        generateHelp rrule newWindow next acc


{-| The window in which events can occur for any given Interval

Take for instance RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE,FR
Mon, Wed, Fri every other week.

The window will be a week long, but will skip a week when the next window
is computed.

-}
type alias Window =
    { lowerBound : Posix, upperBound : Posix }


computeWindow : Recurrence -> Posix -> Window
computeWindow rrule lowerBound =
    { lowerBound = lowerBound
    , upperBound =
        Time.Extra.ceiling (Util.weekdayToInterval rrule.weekStart)
            rrule.tzid
            lowerBound
            |> Util.subtract 1
    }


computeNextWindow : Recurrence -> Window -> Window
computeNextWindow rrule window =
    let
        timeUnit =
            Util.freqToInterval rrule.frequency

        newUpperBound =
            window.upperBound
                |> Util.add 1
                |> Time.Extra.add timeUnit rrule.interval rrule.tzid

        newLowerBound =
            newUpperBound
                |> Time.Extra.add timeUnit -1 rrule.tzid
    in
    { lowerBound = newLowerBound
    , upperBound = newUpperBound |> Util.subtract 1
    }


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


{-| "same or greater than the frequency _limit_ the number of occurrences"
-}
checkGTE : Recurrence -> Posix -> Bool
checkGTE rrule time =
    -- TODO support negative numbers checks for "last x of y"
    [ List.member (Util.toWeekNo rrule.tzid time) rrule.byWeekNo
    , List.member (Time.toMonth rrule.tzid time |> Date.monthToNumber) rrule.byMonth
    , List.member (Time.toDay rrule.tzid time) rrule.byMonthDay
    ]
        |> List.any identity
        |> not


checkLT : Recurrence -> Posix -> Bool
checkLT rrule time =
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


maxInterval : Recurrence -> Interval
maxInterval rrule =
    if not (List.isEmpty rrule.byDay) then
        Day

    else
        Week


bumpToNextWindow : Recurrence -> Posix -> Posix
bumpToNextWindow rrule time =
    let
        timeParts =
            time |> Time.Extra.posixToParts rrule.tzid

        nextParts =
            Time.Extra.add (Util.freqToInterval rrule.frequency)
                rrule.interval
                rrule.tzid
                time
                |> Time.Extra.floor (Util.freqToInterval rrule.frequency)
                    rrule.tzid
                |> Time.Extra.posixToParts rrule.tzid
    in
    { timeParts
        | year = nextParts.year
        , month = nextParts.month
        , day = nextParts.day
    }
        |> Time.Extra.partsToPosix rrule.tzid
