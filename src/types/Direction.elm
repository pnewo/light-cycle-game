module Types.Direction exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


directionToInt : Direction -> Int
directionToInt direction =
    case direction of
        Up ->
            0

        Right ->
            1

        Down ->
            2

        Left ->
            3


intToDirection : Int -> Direction
intToDirection int =
    case int of
        (-1) ->
            Left

        0 ->
            Up

        1 ->
            Right

        2 ->
            Down

        3 ->
            Left

        _ ->
            Up
