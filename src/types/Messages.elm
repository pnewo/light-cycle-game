module Types.Messages exposing (..)

import Time exposing (Time)


type Msg
    = Move Bool Float
    | TurnPress Int
    | Paused
    | RandomBool Time
    | RandomFloat Bool
    | AnimationTick Time
    | NoOp
