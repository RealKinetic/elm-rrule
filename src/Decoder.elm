module Decoder exposing (..)

import Date exposing (Date)
import Dict
import Either exposing (Either(..))
import Parser as P exposing (..)
import Recurrence exposing (Frequency(..), Recurrence, UntilCount(..))
import Time exposing (Posix, Weekday(..), Zone)
import Time.Extra as TE
import TimeZone



{-
   TODO - All day recurring events do not have a timezone.... How do I handle this.
-}


run : List String -> Result Error Recurrence
run rrules =
    getDTSTART
        |> andThen (\{ start, zone } -> runHelp zone start)
        |> decodeLines rrules


runWithDTSTART : List String -> Zone -> Posix -> Result Error Recurrence
runWithDTSTART rrules zone start =
    runHelp zone start
        |> decodeLines rrules


type alias RawRRules =
    { dtStart : DTSTART
    , rrule_ : RRULE
    , exdate_ : EXDATE
    , rdate_ : RDATE
    }


runHelp : Zone -> Posix -> Decoder Recurrence
runHelp zone start =
    succeed RawRRules
        |> andMap (succeed <| DTSTART zone start)
        |> andMap getRRULE
        |> andMap getEXDATE
        |> andMap getRDATE
        |> andThen rawRRulesToRecurrence


rawRRulesToRecurrence : RawRRules -> Decoder Recurrence
rawRRulesToRecurrence { dtStart, rrule_, exdate_, rdate_ } =
    succeed Recurrence
        |> andMap (frequency rrule_)
        |> andMap (weekStart rrule_)
        |> andMap (interval rrule_)
        |> andMap (succeed dtStart.start)
        |> andMap (succeed dtStart.zone)
        |> andMap (untilCount rrule_)
        |> andMap (byDay rrule_)
        |> andMap (byMonthDay rrule_)
        |> andMap (byMonth rrule_)
        |> andMap (byWeekNo rrule_)
        |> andMap (byYearDay rrule_)
        |> andMap (exdates exdate_)
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
        |. symbol "="
        |= (chompUntilEndOr ";"
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
            |. symbol "="
            |= weekday
            |> runParserOn rrule
        , succeed Mon
        ]


byDay : RRULE -> Decoder (List (Either ( Int, Weekday ) Weekday))
byDay (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYDAY"
            |. symbol "="
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
            |. symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


byMonth : RRULE -> Decoder (List Int)
byMonth (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYMONTH"
            |. symbol "="
            |= list P.int
            |> runParserOn rrule
        , succeed []
        ]


byWeekNo : RRULE -> Decoder (List Int)
byWeekNo (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYWEEKNO"
            |. symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


byYearDay : RRULE -> Decoder (List Int)
byYearDay (RRULE rrule) =
    oneOf
        [ P.succeed identity
            |. chompThrough "BYYEARDAY"
            |. symbol "="
            |= list parseNegatableInt
            |> runParserOn rrule
        , succeed []
        ]


untilCount : RRULE -> Decoder (Maybe UntilCount)
untilCount rrule =
    oneOf [ until rrule, count rrule ]
        |> maybe


{-| UNTIL=19971224T000000Z
-}
until : RRULE -> Decoder UntilCount
until (RRULE rrule) =
    P.succeed identity
        |. chompThrough "UNTIL"
        |. symbol "="
        {- TODO this can be DATE as well as DATETIME pg 41 of the spec
           The UNTIL rule part defines a DATE or DATE-TIME value that bounds
           the recurrence rule in an inclusive manner.
        -}
        |= parseDateTime
        |> P.map (TE.partsToPosix Time.utc >> Until)
        |> runParserOn rrule


{-| COUNT=42
-}
count : RRULE -> Decoder UntilCount
count (RRULE rrule) =
    P.succeed identity
        |. chompThrough "COUNT"
        |. symbol "="
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
            |. symbol "="
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
            problem ("Unknown FREQ: " ++ str)


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
                        problem ("Unknown FREQ: " ++ str)
            )


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
        |. symbol "="
        |= (chompUntil ":"
                |> P.getChompedString
                |> P.andThen parseTzid
           )
        |. symbol ":"
        -- TODO this can be DATE as well as DATETIME pg 41 of the spec
        |= parseDateTime
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


parseTzid : String -> Parser Zone
parseTzid ianaZoneName =
    case Dict.get ianaZoneName ianaTimezones of
        Just zone ->
            P.succeed <| zone ()

        Nothing ->
            P.problem <| "Unknown IANA zone: " ++ ianaZoneName


{-| "EXDATE;TZID=America/Denver:20170419T083000,2017052…0,20190717T090000,20190718T090000,20190730T090000"
-}
type EXDATE
    = EXDATE String


getEXDATE : Decoder EXDATE
getEXDATE =
    Decoder <| find False EXDATE "EXDATE"


exdates : EXDATE -> Decoder (List Posix)
exdates (EXDATE exdateString) =
    -- TODO Default to UTC if TZID doesn't exist?
    P.oneOf
        [ P.succeed Tuple.pair
            |. chompThrough "TZID"
            |. symbol "="
            |= (chompUntil ":"
                    |> P.getChompedString
                    |> P.andThen parseTzid
               )
            |. symbol ":"
            |= list parseDateTime
            |> P.map
                (\( zone, exdates_ ) ->
                    List.map (TE.partsToPosix zone) exdates_
                )
        , P.succeed []
        ]
        |> runParserOn exdateString



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
            [ P.succeed Loop |. symbol ","
            , P.succeed Done |. symbol ";"
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


{-| Properties need to be separated by newline

    e.g. "RRULE:UNTIL=.....;\nEXDATE:TZID=...."

-}
decodeLines : List String -> Decoder a -> Result Error a
decodeLines lines (Decoder decoderF) =
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
helper length count_ =
    if length == count_ then
        P.succeed ()
            |> P.map (\_ -> Done count_)

    else
        P.succeed (Loop (count_ + 1))
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
