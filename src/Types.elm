module Types exposing (..)

import Dict exposing (Dict)

type alias Model = {determined: Dict Position Int --lights position and statas
                   ,undetermined: List Position
                   ,matrix: Dict (Int, Int) Int --matrix (row, col) value
                   ,size: Int
                   ,p: Int
                   }
type alias Position = (Int, Int, Int)
type alias Point = {x:Int, y:Int}

type Msg = Determine
    | Complete
    | Reset
    | RandGenerated (List Int)
    | Clicked Position
    

    

