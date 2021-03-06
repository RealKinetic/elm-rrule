module Util exposing (..)

import Date exposing (Date)
import Set
import Time exposing (Month(..), Posix, Weekday(..), Zone)
import Time.Extra as TE exposing (Interval(..))


notEmpty : List a -> Bool
notEmpty =
    List.isEmpty >> not


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


lte : Posix -> Posix -> Bool
lte t1 t2 =
    Time.posixToMillis t1 <= Time.posixToMillis t2


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


year2250 : Posix
year2250 =
    Time.millisToPosix 8845394400000


{-| Merge time of the first posix arg to the date of the second.


# 1997-05-19T13:15-0400 - 1997-05-27T00:00-0400 == 1997-05-27T13:15-0400

    mergeTimeOf nyc (Posix 864062100000) (Posix 864705600000)
        == 864753300000

-}
mergeTimeOf : Zone -> Posix -> Posix -> Posix
mergeTimeOf zone timeOf dateOf =
    let
        ( timeOfParts, dateOfParts ) =
            ( TE.posixToParts zone timeOf
            , TE.posixToParts zone dateOf
            )
    in
    { timeOfParts
        | year = dateOfParts.year
        , month = dateOfParts.month
        , day = dateOfParts.day
    }
        |> TE.partsToPosix zone


{-| Taken from justinmimbs/date
-}
daysInMonth : Int -> Month -> Int
daysInMonth year month_ =
    case month_ of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


daysInYear : Int -> Int
daysInYear year =
    if isLeapYear year then
        366

    else
        365


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


daysBeforeMonth : Int -> Month -> Int
daysBeforeMonth y m =
    let
        leapDays =
            if isLeapYear y then
                1

            else
                0
    in
    case m of
        Jan ->
            0

        Feb ->
            31

        Mar ->
            59 + leapDays

        Apr ->
            90 + leapDays

        May ->
            120 + leapDays

        Jun ->
            151 + leapDays

        Jul ->
            181 + leapDays

        Aug ->
            212 + leapDays

        Sep ->
            243 + leapDays

        Oct ->
            273 + leapDays

        Nov ->
            304 + leapDays

        Dec ->
            334 + leapDays


{-| Adapted from justinmimbs/date library
<https://github.com/justinmimbs/date/blob/29573e0550848e9dd62d71c32bd1890d3bc43461/src/Date.elm>
-}
weekNumber : Weekday -> Date -> Int
weekNumber weekStart date =
    let
        rd =
            Date.toRataDie date

        wdn =
            weekdayNumber weekStart date

        wy =
            -- `year <thursday of this week>`
            Date.year (Date.fromRataDie (rd + (4 - wdn)))

        week1Day1 =
            daysBeforeWeekYear weekStart wy + 1
    in
    1 + (rd - week1Day1) // 7


is53WeekYear : Weekday -> Int -> Bool
is53WeekYear weekStart year =
    let
        wdnJan1 =
            weekdayNumber weekStart (firstOfYear year)
    in
    -- any year starting on 4th day of week
    -- or any leap year starting on 3rd day of week
    wdnJan1 == 4 || (wdnJan1 == 3 && isLeapYear year)


{-| The weekday number (1–7)
Depends on weekstart, which can vary.

Monday - Europe / India / Russia / Etc
Sunday - Americas / China / Japan / South Africa / Zimbabwe / Phillipines / South Korea
Saturday - Islamic Countries

<http://chartsbin.com/view/41671>

-}
weekdayNumber : Weekday -> Date -> Int
weekdayNumber weekStart date =
    -- TODO There's a bug for finding the weekday on Jan 1 2001 which
    -- Util.weekNumber Time.Mon (Date.fromPosix nyc <| Time.millisToPosix 978307200000)
    let
        -- Rata Die base date is a Monday
        -- so we must slide the result to fit our WKST
        adjust rd =
            case weekStart of
                Mon ->
                    rd

                Sun ->
                    rd + 1

                _ ->
                    rd + 2
    in
    case Date.toRataDie date |> adjust |> modBy 7 of
        0 ->
            7

        n ->
            n


firstOfYear : Int -> Date
firstOfYear y =
    Date.fromRataDie <| daysBeforeYear y + 1


daysBeforeWeekYear : Weekday -> Int -> Int
daysBeforeWeekYear weekStart y =
    let
        jan4 =
            daysBeforeYear y + 4
    in
    jan4 - weekdayNumber weekStart (Date.fromRataDie jan4)


daysBeforeYear : Int -> Int
daysBeforeYear y1 =
    let
        y =
            y1 - 1

        leapYears =
            floorDiv y 4 - floorDiv y 100 + floorDiv y 400
    in
    365 * y + leapYears


floorDiv : Int -> Int -> Int
floorDiv a b =
    Basics.floor (toFloat a / toFloat b)


{-| TODO - Use dedupe optimization w/ sort
<https://discourse.elm-lang.org/t/remove-duplications-from-the-list-reviewed/6529>
-}
dedupeAndSortTimes : List Posix -> List Posix
dedupeAndSortTimes times =
    List.foldl (\time set -> Set.insert (Time.posixToMillis time) set) Set.empty times
        |> Set.toList
        |> List.sort
        |> List.map Time.millisToPosix
