module Decoder exposing (..)

import Date exposing (Date)
import Dict
import Parser as P exposing (..)
import Recurrence exposing (Frequency(..), Recurrence, UntilCount)
import Time exposing (Posix, Zone)
import Time.Extra as TE
import TimeZone



--type alias Recurrence =
--    { -- Required
--      frequency : Frequency
--    , weekStart : Weekday
--    , interval : Int
--    , dtStart : Posix
--    , tzid : Zone
--
--    -- Optional
--    , untilCount : Maybe UntilCount
--    , byDay : List (Either ( Int, Weekday ) Weekday)
--    , byMonthDay : List Int
--    , byMonth : List Int
--    , byWeekNo : List Int
--    , exdates : Set Posix
--    }


thingDecoder =
    getRRULE
        |> andThen
            (\rrule_ ->
                succeed Recurrence
                    |> andMap (maybe exdate)
                    |> andMap getDTSTART
                    |> andMap (succeed rrule_)
                    |> andMap (frequency rrule_)
            )


{-| "RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20200120T065959Z;BYDAY=MO,TU,WE,TH"
-}
type RRULE
    = RRULE String


getRRULE : Decoder RRULE
getRRULE =
    Decoder <| find RRULE "RRULE"


frequency : RRULE -> Decoder Frequency
frequency (RRULE rrule) =
    P.succeed identity
        |. chompThrough "FREQ"
        |. symbol "="
        |= (chompUntilEndOr ";"
                |> P.getChompedString
                |> P.andThen parseFreq
           )
        |> runParserOn rrule


untilCount : RRULE -> Decoder (Maybe UntilCount)
untilCount rrule =
    oneOf [ until rrule, count rrule ]
        |> maybe


until : RRULE -> Decoder UntilCount
until (RRULE rrule) =
    P.succeed identity
        |. chompThrough "FREQ"
        |. symbol "="
        |= (chompUntilEndOr ";"
                |> P.getChompedString
                |> P.andThen parseFreq
           )
        |> runParserOn rrule


count : RRULE -> Decoder UntilCount
count (RRULE rrule) =
    P.succeed identity
        |. chompThrough "FREQ"
        |. symbol "="
        |= (chompUntilEndOr ";"
                |> P.getChompedString
                |> P.andThen parseFreq
           )
        |> runParserOn rrule


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
    P.run (parseDateTime |. symbol "Z" |. end) "20190806T055959Z"


{-| DTSTART;TZID=America/Denver:20190603T090000;
-}
type DTSTART
    = DTSTART String


getDTSTART : Decoder DTSTART
getDTSTART =
    Decoder <| find DTSTART "DTSTART"



-- TODO Default to UTC if TZID doesn't exist?


tzidAndDtStart : DTSTART -> Decoder ( Zone, Posix )
tzidAndDtStart (DTSTART dtstart_) =
    P.succeed Tuple.pair
        |. chompThrough "TZID"
        |. symbol "="
        |= (chompUntil ":"
                |> P.getChompedString
                |> P.andThen parseTzid
           )
        |. symbol ":"
        |= parseDateTime
        |> P.map
            (\( zone, parts ) ->
                ( zone, TE.partsToPosix zone parts )
            )
        |> runParserOn dtstart_


parseTzid : String -> Parser Zone
parseTzid ianaZoneName =
    case Dict.get ianaZoneName TimeZone.zones of
        Just zone ->
            P.succeed <| zone ()

        Nothing ->
            P.problem <| "Unknown IANA zone: " ++ ianaZoneName


{-| "EXDATE;TZID=America/Denver:20170419T083000,2017052…0,20190717T090000,20190718T090000,20190730T090000"
-}
type EXDATE
    = EXDATE String


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
    | OneOf (List Error)
    | Failure String String


{-| Properties need to be separated by newline

    e.g. "RRULE:UNTIL=.....;\nEXDATE:TZID=...."

-}
run : Decoder a -> List String -> Result Error a
run (Decoder decoderF) lines =
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
                    run (toB decoded) val

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



{- Parsing -}


{-| -}
chompThrough str =
    chompUntil str
        |. keyword str


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
