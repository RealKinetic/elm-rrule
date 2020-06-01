module Monthly exposing (..)

import Examples.Monthly as Monthly
import Helper exposing (benchmarkExamples)


main =
    benchmarkExamples "Monthly"
        [ Monthly.example1
        , Monthly.example2
        , Monthly.example3
        , Monthly.example4
        , Monthly.example5
        , Monthly.example6
        , Monthly.example7
        , Monthly.example8
        , Monthly.example9
        , Monthly.example10
        , Monthly.example11
        ]



{- 5/31/20 - d8e897a

   Monthly.example1     = 321 r/s
   Monthly.example2     =  r/s
   Monthly.example3     =  r/s
   Monthly.example4     = 515 r/s
   Monthly.example5     =  r/s
   Monthly.example6     =  r/s
   Monthly.example7     =  r/s
   Monthly.example8     =  r/s
   Monthly.example9     =  r/s
   Monthly.example10    =  r/s
   Monthly.example11    =  r/s

-}
