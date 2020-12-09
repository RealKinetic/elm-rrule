module Between exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Examples.Between as Between
import RRule exposing (RRule)


main =
    benchmarkExamples "Between"
        [ Between.example1
        , Between.example2
        , Between.example3
        , Between.example4
        ]


benchmarkExamples : String -> List Between.Example -> BenchmarkProgram
benchmarkExamples desc examples =
    List.map benchmarkExample examples
        |> describe desc
        |> program


benchmarkExample : Between.Example -> Benchmark
benchmarkExample { description, recurrence, window } =
    benchmark description <| \_ -> RRule.between window recurrence
