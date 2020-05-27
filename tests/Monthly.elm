module Monthly exposing (..)

import Common
import Either exposing (Either(..))
import Expect
import Interval.Monthly as Monthly
import RRule exposing (Frequency(..), Recurrence, UntilCount(..))
import Test exposing (..)
import Time exposing (Weekday(..))
import TimeZone


suite : Test
suite =
    []
        |> List.map (Common.toTest Monthly.generate)
        |> describe "Monthly Tests"
