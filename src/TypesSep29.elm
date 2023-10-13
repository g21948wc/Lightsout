module TypesSep29 exposing (..)

import Dict exposing (Dict)

type alias Model = {determined: Dict Position State -- lights position and status
                   ,undetermined: List Position
                   ,matrix: Dict (Int, Int) Int -- matrix (row, col) value  
                   ,size: Int
                   ,p: Int
                   }
type alias Position = (Int, Int, Int)
type alias Point = {x:Int, y:Int}
type alias State = {brightness: Int, numClick: Maybe Int}

type Msg = Determine
    | Complete
    | Reset
    | RandGenerated (List Int)
    | Clicked Position
    | Solve

    

