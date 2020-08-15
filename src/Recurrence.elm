module Recurrence exposing (..)

import Date exposing (Date, Month)
import Either exposing (Either(..))
import Time exposing (Posix, Weekday(..), Zone)


type alias Recurrence =
    { -- Required
      frequency : Frequency
    , weekStart : Weekday
    , interval : Int
    , dtStart : Posix

    -- TODO Support floating times by having the user provide a fallback IANA?
    , tzid : Zone

    -- Optional
    , untilCount : Maybe UntilCount
    , byDay : List (Either ( Int, Weekday ) Weekday)
    , byMonthDay : List Int
    , byMonth : List Int
    , byWeekNo : List Int
    , byYearDay : List Int
    , exdates : List Posix
    , rdates : List Posix

    -- Currently not supported
    --, bySecond : List Int
    --, byMinute : List Int
    --, byHour : List Int
    --, bySetPos : List Int
    }


default : Recurrence
default =
    { frequency = Weekly
    , weekStart = Mon
    , interval = 1
    , dtStart = Time.millisToPosix 0
    , tzid = Time.utc
    , untilCount = Nothing
    , byDay = []
    , byMonthDay = []
    , byMonth = []
    , byWeekNo = []
    , byYearDay = []
    , exdates = []
    , rdates = []
    }


type Frequency
    = Daily
    | Weekly
    | Monthly
    | Yearly


type UntilCount
    = Count Int
      -- TODO Support Date and DateTime
    | Until Posix


type Time
    = DateTime Posix
    | Date Date


{-| Information, not contained in the rule, necessary to determine the
various recurrence instance start time and dates are derived from
the Start Time ("DTSTART") component attribute.

If the BYDAY, BYMONTHDAY, or BYMONTH rule part are missing,
the appropriate day or month are retrieved from the "DTSTART" property.

For example, "FREQ=YEARLY;BYMONTH=1" doesn't specify a specific day
within the month or a time. This information would be the same
as what is specified for "DTSTART".

-}
normalize : Recurrence -> Recurrence
normalize rrule =
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
                      -- TODO Should this be byMonthDay or byDay?
                        | byMonthDay = [ Time.toDay rrule.tzid rrule.dtStart ]
                    }

                _ ->
                    rrule

        Yearly ->
            case ( rrule.byDay, rrule.byMonthDay, rrule.byYearDay ) of
                ( [], [], [] ) ->
                    { rrule
                        | byMonthDay = [ Time.toDay rrule.tzid rrule.dtStart ]
                    }

                _ ->
                    rrule

        Daily ->
            rrule
