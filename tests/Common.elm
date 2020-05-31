module Common exposing (..)

import Expect
import Generator
import RRule exposing (Recurrence)
import Test exposing (Test, describe)
import Time exposing (Posix)


type alias RecurrenceTest =
    { description : String
    , rrule : List String
    , recurrence : Recurrence
    , dates : List Int
    }


test : String -> List RecurrenceTest -> Test
test tipe recurrenceTests =
    describe tipe
        (List.map toTest recurrenceTests)


toTest : RecurrenceTest -> Test
toTest { description, rrule, recurrence, dates } =
    Test.test description <|
        \_ ->
            Generator.run recurrence
                |> Expect.equal (List.map Time.millisToPosix dates)
