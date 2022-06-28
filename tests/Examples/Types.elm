module Examples.Types exposing (..)

import RRule exposing (RRule)


type alias Example =
    { description : String
    , rrule : List String
    , recurrence : RRule
    , dates : List Int
    , text : String
    }
