module Components.GameArea exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import List exposing (indexedMap, repeat)
import Types.Messages exposing (Msg)
import Types.Model exposing (Model)
import Types.Direction exposing (..)
import Types.GameArea exposing (..)


generateEmptyGameArea : List (List Square)
generateEmptyGameArea =
    repeat gameAreaRows (repeat gameAreaCols Empty)


renderGameArea : Model -> Html Msg
renderGameArea model =
    let
        cycleHeadLocation =
            model.cycle.head.location

        cycleHeadDirection =
            model.cycle.head.direction

        cycleTail =
            model.cycle.tail

        enemyCycleHeadLocation =
            model.enemyCycle.head.location

        enemyCycleHeadDirection =
            model.enemyCycle.head.direction

        enemyCycleTail =
            model.enemyCycle.tail

        gameArea : GameArea
        gameArea =
            generateEmptyGameArea
                |> indexedMap
                    (\rowIndex areaRow ->
                        indexedMap
                            (\colIndex square ->
                                if ( rowIndex, colIndex ) == cycleHeadLocation then
                                    CycleHead cycleHeadDirection
                                else if (List.member ( rowIndex, colIndex ) cycleTail) then
                                    CycleTail
                                else if ( rowIndex, colIndex ) == enemyCycleHeadLocation then
                                    EnemyCycleHead enemyCycleHeadDirection
                                else if (List.member ( rowIndex, colIndex ) enemyCycleTail) then
                                    EnemyCycleTail
                                else
                                    square
                            )
                            areaRow
                    )
    in
        div [ class "game-area" ] (indexedMap (\index gameAreaRow -> div [ class "game-area__row" ] (renderGameAreaRow gameAreaRow index)) gameArea)


renderGameAreaRow : GameAreaRow -> Int -> List (Html Msg)
renderGameAreaRow gameAreaRow rowIndex =
    indexedMap
        (\index square ->
            case square of
                CycleHead direction ->
                    div [ class "game-area__cell--cycle-head" ] [ text <| renderCycleHead direction ]

                CycleTail ->
                    div [ class "game-area__cell--cycle-tail" ] [ text "" ]

                EnemyCycleHead direction ->
                    div [ class "game-area__cell--enemy-cycle-head" ] [ text <| renderCycleHead direction ]

                EnemyCycleTail ->
                    div [ class "game-area__cell--enemy-cycle-tail" ] [ text "" ]

                Empty ->
                    div [ class "game-area__cell" ] [ text "" ]
        )
        gameAreaRow


renderCycleHead : Direction -> String
renderCycleHead direction =
    case direction of
        Up ->
            ""

        Right ->
            ""

        Down ->
            ""

        Left ->
            ""
