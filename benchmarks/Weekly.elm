module Weekly exposing (..)

import Examples.Weekly as Weekly
import Helper exposing (benchmarkExamples)


main =
    benchmarkExamples "Weekly"
        [ --Weekly.example1
          --, Weekly.example2
          --, Weekly.example3
          --, Weekly.example4_1
          --, Weekly.example4_2
          --,
          Weekly.example5

        --, Weekly.example6
        ]



{- 5/31/20 - d8e897a

   Weekly.example1     = 3,478 r/s
   Weekly.example2     = 2,179 r/s
   Weekly.example3     = 3,275 r/s
   Weekly.example4_1   = 2,361 r/s
   Weekly.example4_2   = 2,696 r/s
   Weekly.example5     = 1,423 r/s
   Weekly.example6     = 3,393 r/s

-}
