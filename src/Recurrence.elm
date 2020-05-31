module Recurrence exposing (..)

import Date exposing (Date, Month)
import Either exposing (Either)
import Parser as P exposing (..)
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra as TE


type alias Thing =
    { exdate : Maybe EXDATE
    , dtstart : DTSTART
    , rrule : RRULE
    , freq : Frequency
    }


thingDecoder =
    rrule
        |> andThen
            (\rrule_ ->
                succeed Thing
                    |> andMap (maybe exdate)
                    |> andMap dtstart
                    |> andMap (succeed rrule_)
                    |> andMap (frequency rrule_)
            )



-- API


type alias Recurrence =
    { -- Required
      weekStart : Weekday
    , frequency : Frequency
    , interval : Int
    , dtStart : Posix
    , tzid : Zone

    -- Optional
    , untilCount : Maybe UntilCount
    , byDay : List (Either ( Int, Weekday ) Weekday)
    , byWeekNo : List Int
    , byMonthDay : List Int
    , byMonth : List Int
    , exdates : Maybe (List Posix)

    -- Not supporting yet
    --, bySecond : List Int
    --, byMinute : List Int
    --, byHour : List Int
    --, byYearDay : List Int
    --, bySetPos : List Int
    }


defaultRRule =
    { frequency = Weekly
    , untilCount = Nothing
    , interval = 1
    , byDay = []
    , byMonthDay = []
    , byYearDay = []
    , byWeekNo = []
    , byMonth = []
    , bySetPos = []
    , weekStart = Mon -- Default according to iCalendar spec
    }


type Frequency
    = Daily
    | Weekly
    | Monthly
    | Yearly


type Time
    = DateTime Posix
    | Date Date


type UntilCount
    = Count Int
      -- TODO Support Date and DateTime
    | Until Posix


type RRULE
    = RRULE String


type DTSTART
    = DTSTART String


type EXDATE
    = EXDATE String



{- RRULE -}


rrule : Decoder RRULE
rrule =
    Decoder <| find RRULE "RRULE"


frequency : RRULE -> Decoder Frequency
frequency (RRULE rrule_) =
    P.succeed identity
        |. chompThrough "FREQ"
        |. symbol "="
        |= (chompUntilEndOr ";"
                |> P.getChompedString
                |> P.andThen parseFreq
           )
        |> runParserOn rrule_


parseFreq : String -> Parser Frequency
parseFreq str =
    case str of
        "DAILY" ->
            P.succeed Daily

        --
        --"WEEKLY" ->
        --    P.succeed Weekly
        --
        --"MONTHLY" ->
        --    P.succeed Monthly
        --
        --"YEARLY" ->
        --    P.succeed Yearly
        _ ->
            problem ("Unknown FREQ: " ++ str)


{-| "20190806T055959"
-}
parseDateTime : Parser TE.Parts
parseDateTime =
    P.succeed TE.Parts
        |= chompDigits 4
        |= (chompDigits 2 |> P.map Date.numberToMonth)
        |= chompDigits 2
        |. symbol "T"
        |= chompDigits 2
        |= chompDigits 2
        |= chompDigits 2
        |= P.succeed 0


{-| "20190806"
-}
parseDate : Parser Date
parseDate =
    P.succeed Date.fromCalendarDate
        |= chompDigits 4
        |= (chompDigits 2 |> P.map Date.numberToMonth)
        |= chompDigits 2


testdt =
    run (parseDateTime |. symbol "Z" |. end) "20190806T055959Z"



{- DTSTART -}


dtstart : Decoder DTSTART
dtstart =
    Decoder <| find DTSTART "DTSTART"



{- EXDATE -}


exdate : Decoder EXDATE
exdate =
    Decoder <| find EXDATE "EXDATE"



{- Helper -}


{-| Support multiple RRULE, EXDATES, RDATES in a recurrence list?

EXRULES...? Ha. No.

-}
find : (String -> a) -> String -> List String -> Result Error a
find tagger name vals =
    case vals of
        [] ->
            Err <| NotFound name

        x :: xs ->
            if String.startsWith name x then
                Ok <| tagger x

            else
                find tagger name xs


{-| Decoder

See <https://github.com/zwilias/elm-json-in-elm/blob/master/src/Json/Decoder.elm>
for nearly identical patterns.

-}
type Decoder a
    = Decoder (List String -> Result Error a)


type Error
    = NotFound String
    | ParseFailure String


{-| Properties need to be separated by "\\n"

e.g. "RRULE:UNTIL=.....;\\nEXDATE:TZID=...."

-}
decodeString : Decoder a -> String -> Result Error a
decodeString (Decoder decoderF) string =
    decoderF (String.lines string)


decodeList : Decoder a -> List String -> Result Error a
decodeList (Decoder decoderF) lines =
    decoderF lines


succeed : a -> Decoder a
succeed val =
    Decoder (\_ -> Ok val)


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoderF) =
    Decoder <|
        \val -> decoderF val |> Result.map f


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
                    decodeList (toB decoded) val

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



{- Parsing -}


{-| -}
chompThrough str =
    chompUntil str |. keyword str


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
            problem (str ++ " is not an int")


chompDigits : Int -> Parser Int
chompDigits length =
    chompChars length
        |> P.andThen stringToInt


chompChars : Int -> Parser String
chompChars length =
    getChompedString <|
        loop 0 (helper length)


helper : Int -> Int -> Parser (Step Int Int)
helper length count =
    if length == count then
        P.succeed ()
            |> P.map (\_ -> Done count)

    else
        P.succeed (Loop (count + 1))
            |. chompIf (\_ -> True)


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
            "Expecting " ++ str ++ "at " ++ position

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
