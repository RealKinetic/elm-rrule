module Helper exposing (..)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Examples.Types exposing (Example)
import RRule


benchmarkExamples : String -> List Example -> BenchmarkProgram
benchmarkExamples desc examples =
    List.map benchmarkExample examples
        |> describe desc
        |> program


benchmarkExample : Example -> Benchmark
benchmarkExample { description, rrule, recurrence, dates } =
    benchmark description <| \_ -> RRule.all recurrence


benchmarkExampleGroup : List Example -> BenchmarkProgram
benchmarkExampleGroup examples =
    always (List.map (\{ recurrence } -> RRule.all recurrence) examples)
        |> benchmark "Group of rrules"
        |> program
