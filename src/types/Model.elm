module Types.Model exposing (..)

import Types.Cycle exposing (Cycle)
import Time exposing (Time)


type alias Model =
    { cycle : Cycle
    , enemyCycle : Cycle
    , paused : Bool
    , turnBuffer : List Int
    , score : Int
    , topScores : List Int
    , gameResult : String
    , elapsedFromTick : Time
    }
