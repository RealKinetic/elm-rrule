module Common exposing (..)

import Expect
import RRule exposing (Recurrence)
import Test exposing (Test, test)
import Time exposing (Posix)


type alias RecurrenceTest =
    { description : String
    , rrule : List String
    , recurrence : Recurrence
    , dates : List Int
    }


type alias Generator =
    Recurrence -> List Posix


toTest : Generator -> RecurrenceTest -> Test
toTest generator { description, rrule, recurrence, dates } =
    test description <|
        \_ ->
            generator recurrence
                |> Expect.equal (List.map Time.millisToPosix dates)
