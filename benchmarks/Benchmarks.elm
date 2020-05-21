module Benchmarks exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (program)
import Generator as G


main =
    program suite


suite : Benchmark
suite =
    describe "Weekly - Mon, Tue, Thur, Fri"
        [ benchmark "Expand 50 years to 2069" <|
            \_ -> G.weekly G.testRule
        ]
