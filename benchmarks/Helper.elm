module Helper exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Examples.Example exposing (Example)
import Generator


benchmarkExamples : String -> List Example -> BenchmarkProgram
benchmarkExamples desc examples =
    List.map benchmarkExample examples
        |> describe desc
        |> program


benchmarkExample : Example -> Benchmark
benchmarkExample { description, rrule, recurrence, dates } =
    benchmark description <| \_ -> Generator.run recurrence
