module Types.Model exposing (..)

import Types.Cycle exposing (Cycle)


type alias Model =
    { cycle : Cycle
    , enemyCycle : Cycle
    , paused : Bool
    , turnBuffer : List Int
    , score : Int
    , topScores : List Int
    , gameResult : String
    }
