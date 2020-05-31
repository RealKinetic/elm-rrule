module Examples.Example exposing (..)

import Recurrence exposing (Recurrence)


type alias Example =
    { description : String
    , rrule : List String
    , recurrence : Recurrence
    , dates : List Int
    }
