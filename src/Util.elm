module Util exposing (..)

import RRule exposing (Frequency(..))
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra exposing (Interval(..))


toWeekNo : Zone -> Posix -> Int
toWeekNo zone time =
    Time.toDay zone time
        |> modBy 7


freqToInterval : Frequency -> Time.Extra.Interval
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


weekdayToInterval : Weekday -> Time.Extra.Interval
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
