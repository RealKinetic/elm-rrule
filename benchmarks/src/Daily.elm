module Daily exposing (..)

import Examples.Daily as Daily
import Helper exposing (benchmarkExamples)


main =
    benchmarkExamples "Daily"
        [ Daily.example1
        , Daily.example2
        , Daily.example3
        , Daily.example4
        , Daily.example5
        ]



{- 5/31/20 - d8e897a

   Daily.example1     = 3,620 r/s
   Daily.example2     =   555 r/s
   Daily.example3     = 3,632 r/s
   Daily.example4     = 6,387 r/s
   Daily.example5     =    54 r/s

-}
