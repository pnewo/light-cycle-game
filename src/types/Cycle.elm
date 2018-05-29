module Types.Cycle exposing (..)

import Types.Direction exposing (Direction)
import Types.Location exposing (Location)


type alias Cycle =
    { head :
        CycleHead
    , tail : CycleTail
    }


type alias CycleHead =
    { direction : Direction
    , location : Location
    }


type alias CycleTail =
    List Location
