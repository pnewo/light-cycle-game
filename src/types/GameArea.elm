module Types.GameArea exposing (..)

import Set exposing (..)
import Types.Direction exposing (..)
import Types.Location exposing (..)
import Types.Cycle exposing (CycleHead)


gameAreaRows : Int
gameAreaRows =
    25


gameAreaCols : Int
gameAreaCols =
    50


type GameOver
    = Win
    | Lose
    | Draw


type Square
    = CycleHead Direction
    | CycleTail
    | EnemyCycleHead Direction
    | EnemyCycleTail
    | Empty


type alias GameAreaRow =
    List Square


type alias GameArea =
    List GameAreaRow


turnDirection : Int -> Direction -> Direction
turnDirection turn direction =
    direction
        |> directionToInt
        |> (\dirInt -> dirInt + turn)
        |> intToDirection


isCrash : List Location -> Location -> Bool
isCrash tailLocations ( row, col ) =
    row
        < 0
        || row
        >= gameAreaRows
        || col
        < 0
        || col
        >= gameAreaCols
        || List.member ( row, col ) tailLocations


isGameOver : Location -> List Location -> Location -> List Location -> Maybe GameOver
isGameOver playerLocation playerTail enemyLocation enemyTail =
    let
        bothTails =
            List.append playerTail enemyTail

        playerCrashLocations =
            enemyLocation :: bothTails

        enemyCrashLocations =
            playerLocation :: bothTails

        playerCrash =
            isCrash playerCrashLocations playerLocation

        enemyCrash =
            isCrash enemyCrashLocations enemyLocation

        maybeCrash =
            if playerCrash && enemyCrash then
                Just Draw
            else if playerCrash then
                Just Lose
            else if enemyCrash then
                Just Win
            else
                Nothing
    in
        maybeCrash


updateHeadLocation : Location -> Direction -> Location
updateHeadLocation location direction =
    let
        sum : Int -> Int -> Int
        sum int1 int2 =
            int1 + int2

        add =
            sum 1

        sub =
            sum -1
    in
        case direction of
            Up ->
                Tuple.mapFirst sub location

            Right ->
                Tuple.mapSecond add location

            Down ->
                Tuple.mapFirst add location

            Left ->
                Tuple.mapSecond sub location


updateEnemyDirection : CycleHead -> List Location -> Bool -> Float -> Direction
updateEnemyDirection { location, direction } blockedLocationList randomBool randomFloat =
    let
        doRandomTurn =
            0.03 > randomFloat

        updateHeadInDirection : Direction -> Location
        updateHeadInDirection =
            updateHeadLocation location

        isLocationAccepted : Location -> Bool
        isLocationAccepted =
            isCrash blockedLocationList
                >> not

        isLocationInDirectionAccepted : Direction -> Bool
        isLocationInDirectionAccepted =
            updateHeadInDirection
                >> isLocationAccepted

        canGoStraight =
            direction
                |> isLocationInDirectionAccepted

        directionLeft =
            turnDirection -1 direction

        locationOnLeft =
            directionLeft
                |> updateHeadInDirection

        canGoLeft =
            directionLeft
                |> isLocationInDirectionAccepted

        directionRight =
            turnDirection 1 direction

        locationOnRight =
            directionRight
                |> updateHeadInDirection

        canGoRight =
            directionRight
                |> isLocationInDirectionAccepted

        newDirection =
            case ( canGoStraight, canGoLeft, canGoRight, doRandomTurn ) of
                ( True, True, True, True ) ->
                    if randomBool then
                        directionLeft
                    else
                        directionRight

                ( True, True, False, True ) ->
                    directionLeft

                ( True, False, True, True ) ->
                    directionRight

                ( True, _, _, False ) ->
                    direction

                ( False, True, True, _ ) ->
                    let
                        ( leftSize, rightSize ) =
                            leftAndRightArea locationOnLeft locationOnRight (location :: blockedLocationList)

                        debug =
                            ( leftSize, rightSize )
                                |> toString
                                |> Debug.log "left - right"

                        debugbool =
                            randomBool
                                |> toString
                                |> Debug.log "random"
                    in
                        if leftSize > rightSize then
                            directionLeft
                        else if rightSize > leftSize then
                            directionRight
                        else if randomBool then
                            directionLeft
                        else
                            directionRight

                ( False, True, False, _ ) ->
                    directionLeft

                ( False, False, True, _ ) ->
                    directionRight

                _ ->
                    direction
    in
        newDirection


leftAndRightArea : Location -> Location -> List Location -> ( Int, Int )
leftAndRightArea locationLeft locationRight blockedList =
    let
        newLeft =
            List.singleton locationLeft

        visitedLeft =
            []

        newRight =
            List.singleton locationRight

        visitedRight =
            []

        (movesLeft, movesRight) =
            recursiveLeftAndRightArea
                blockedList
                newLeft
                visitedLeft
                newRight
                visitedRight

    in
        ( Set.size movesLeft, Set.size movesRight )

recursiveLeftAndRightArea : List Location -> List Location -> List (List Location) -> List Location -> List (List Location) -> (Set Location, Set Location)
recursiveLeftAndRightArea blocked movesLeft visitedLeft movesRight visitedRight =
    let
        concatToUniqueList =
            List.concat >> Set.fromList >> Set.toList

        blockedVisitedLeftList =
            concatToUniqueList (blocked :: visitedLeft)

        newMovesLeft =
            movesLeft
                |> List.map (possibleMoves blockedVisitedLeftList)
                |> concatToUniqueList

        blockedVisitedRightList =
            concatToUniqueList (blocked :: visitedRight)

        newMovesRight =
            movesRight
                |> List.map (possibleMoves blockedVisitedRightList)
                |> concatToUniqueList

        newVisitedLeft =
            movesLeft :: visitedLeft

        newVisitedRight =
            movesRight :: visitedRight

        newMovesLeftSet =
            Set.fromList newMovesLeft

        newMovesRightSet =
            Set.fromList newMovesRight
    in
        if (List.isEmpty newMovesLeft) then
            (Set.empty, Set.singleton (0, 0))
        else if (List.isEmpty newMovesRight) then
            (Set.singleton (0, 0), Set.empty)
        else if (not <| Set.isEmpty <| Set.intersect newMovesLeftSet newMovesRightSet)
            || List.length newVisitedLeft > 20
            || List.length newVisitedRight > 20 then
            (newVisitedLeft, newVisitedRight)
                |> Tuple.mapFirst (List.concat >> Set.fromList)
                |> Tuple.mapSecond (List.concat >> Set.fromList)

        else
            recursiveLeftAndRightArea
                blocked
                newMovesLeft
                newVisitedLeft
                newMovesRight
                newVisitedRight



possibleMoves : List Location -> Location -> List Location
possibleMoves blockedList location =
    let
        addToLocation =
            tupleSum location

        moves =
            [ ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ]
                |> List.map addToLocation

        availableLocations =
            isCrash blockedList
                >> not

        possibleMoves =
            moves
                |> List.filter availableLocations
    in
        possibleMoves


tupleSum : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
tupleSum ( int1, int2 ) ( int3, int4 ) =
    ( int1 + int3, int2 + int4 )
