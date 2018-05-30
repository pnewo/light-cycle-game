module Main exposing (..)

import Keyboard exposing (KeyCode)
import AnimationFrame
import Random
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Components.GameArea exposing (..)
import Types.Messages exposing (..)
import Types.Model exposing (..)
import Types.GameArea exposing (..)
import Types.Direction exposing (..)
import Types.Cycle exposing (CycleHead)


---- MODEL ----


gameTick : Float
gameTick =
    250


storedScoresAmount : Int
storedScoresAmount =
    5


initCycleHead : CycleHead
initCycleHead =
    { direction = Right
    , location = ( gameAreaRows // 2, gameAreaCols // 4 )
    }


initEnemyCycleHead : CycleHead
initEnemyCycleHead =
    { direction = Left
    , location = ( gameAreaRows // 2, gameAreaCols // 4 * 3 + 1 )
    }


initModel : Model
initModel =
    { cycle =
        { head = initCycleHead
        , tail = []
        }
    , enemyCycle =
        { head = initEnemyCycleHead
        , tail = []
        }
    , paused = True
    , turnBuffer = []
    , score = 0
    , topScores = List.repeat storedScoresAmount 0
    , gameResult = ""
    , elapsedFromTick = 1
    }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationTick time ->
            let
                elapsed =
                    model.elapsedFromTick + time

                ( newElapsed, command ) =
                    if (elapsed) >= gameTick then
                        ( 0, Random.generate RandomFloat Random.bool )
                    else
                        ( elapsed, Cmd.none )

                newModel =
                    { model
                        | elapsedFromTick = newElapsed
                    }
            in
                ( newModel, command )

        RandomBool _ ->
            ( model
            , Random.generate RandomFloat Random.bool
            )

        RandomFloat randomBool ->
            ( model
            , Random.generate (Move randomBool) (Random.float 0 1)
            )

        Move randomBool randomFloat ->
            let
                cycleHead =
                    model.cycle.head

                ( newDirection, newTurnBuffer ) =
                    case model.turnBuffer of
                        turn :: rest ->
                            ( turnDirection turn model.cycle.head.direction, rest )

                        _ ->
                            ( model.cycle.head.direction, model.turnBuffer )

                newLocation =
                    updateHeadLocation model.cycle.head.location newDirection

                newHead =
                    { cycleHead
                        | location = newLocation
                        , direction = newDirection
                    }

                newTail =
                    model.cycle.head.location
                        :: model.cycle.tail

                newCycle =
                    { head = newHead
                    , tail = newTail
                    }

                enemyHead =
                    model.enemyCycle.head

                newEnemyDirection =
                    updateEnemyDirection
                        model.enemyCycle.head
                        (List.append model.enemyCycle.tail <| newHead.location :: newTail)
                        randomBool
                        randomFloat

                newEnemyLocation =
                    updateHeadLocation model.enemyCycle.head.location newEnemyDirection

                newEnemyHead =
                    { enemyHead
                        | location = newEnemyLocation
                        , direction = newEnemyDirection
                    }

                newEnemyTail =
                    model.enemyCycle.head.location
                        :: model.enemyCycle.tail

                newEnemy =
                    { head = newEnemyHead
                    , tail = newEnemyTail
                    }

                isMaybeGameOver =
                    isGameOver newLocation newTail newEnemyLocation newEnemyTail

                newScore =
                    case isMaybeGameOver of
                        Just Win ->
                            model.score + 1000

                        Just Draw ->
                            model.score + 500

                        _ ->
                            model.score + 1
            in
                case isMaybeGameOver of
                    Just result ->
                        ( { initModel
                            | topScores = updateTopScores model.topScores newScore
                            , gameResult = toString result
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( { model
                            | cycle = newCycle
                            , enemyCycle = newEnemy
                            , turnBuffer = newTurnBuffer
                            , score = newScore
                          }
                        , Cmd.none
                        )

        TurnPress turn ->
            let
                newTurnBuffer =
                    if List.length model.turnBuffer < 2 then
                        List.append model.turnBuffer [ turn ]
                    else
                        model.turnBuffer
            in
                ( { model | turnBuffer = newTurnBuffer }, Cmd.none )

        Paused ->
            ( { model | paused = not model.paused, gameResult = "" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateTopScores : List Int -> Int -> List Int
updateTopScores topScores newScore =
    newScore
        :: topScores
        |> List.sort
        |> List.reverse
        |> List.take storedScoresAmount



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg", onClick Paused ] []
        , h1 [] [ text "Light Cycle" ]
        , renderGameArea model
        , div [] [ text model.gameResult ]
        , div [] [ text <| toString model.score ]
        , renderTopScores model.topScores
        ]


renderTopScores : List Int -> Html Msg
renderTopScores intList =
    div []
        (intList
            |> List.indexedMap (\index score -> div [] [ text <| (toString (index + 1) ++ ". " ++ toString score) ])
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subArr =
            if model.paused then
                [ Sub.none ]
            else
                [ AnimationFrame.diffs AnimationTick
                , Keyboard.downs key
                ]
    in
        Sub.batch subArr


key : KeyCode -> Msg
key keycode =
    case keycode of
        37 ->
            TurnPress -1

        39 ->
            TurnPress 1

        _ ->
            NoOp



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
