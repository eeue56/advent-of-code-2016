module Main

import Data.String as String

data Direction = UUp | DDown | LLeft | RRight

implementation Show Direction where
    show UUp = "Up"
    show DDown = "Down"
    show LLeft = "Left"
    show RRight = "Right"

joinDirection : (start : Direction) -> (second : Direction) -> Direction
joinDirection UUp second = second
joinDirection DDown LLeft = RRight
joinDirection DDown RRight = LLeft

joinDirection _ UUp = UUp
joinDirection _ DDown = DDown

joinDirection LLeft LLeft = DDown
joinDirection LLeft RRight = UUp

joinDirection RRight RRight = DDown
joinDirection RRight LLeft = UUp


record Position where
    constructor Pos
    x : Int
    y : Int
    facing : Direction

Show Position where
    show (Pos x y facing) =
        "x = " ++ show x ++ ", y = "
        ++ show y ++ ", facing = " ++ show facing

distanceBetween : Position -> Position -> Int
distanceBetween first second =
    (x first - x second)  + (y first - y second)


record Move where
    constructor MakeMove
    direction : Direction
    amount : Int

Show Move where
    show (MakeMove d n) = show d ++ " " ++ show n

start : Position
start = Pos 0 0 UUp

moveFromString : String -> Maybe Move
moveFromString str =
    case unpack str of
        'R'::xs =>
            map (MakeMove RRight) $ String.parseInteger $ pack xs
        'L'::xs =>
            map (MakeMove LLeft) $ String.parseInteger $ pack xs
        _ =>
            Nothing


collectMovesFromString : String -> List Move
collectMovesFromString str =
    catMaybes
        $ map moveFromString
        $ map trim
        $ split (== ',') str


moveX : Direction -> Int -> Int -> Int
moveX LLeft x amount = x + amount
moveX RRight x amount = x - amount
moveX _ x _ = x

moveY : Direction -> Int -> Int -> Int
moveY UUp x amount = x + amount
moveY DDown x amount = x - amount
moveY _ x _ = x

moveAlong : Move -> Position -> Position
moveAlong move position =
    record
        { facing = newFace
        , x = newX
        , y = newY
        }
        position
        where
            newFace =
                joinDirection (facing position) (direction move)

            newX = moveX newFace (x position) (amount move)
            newY = moveY newFace (y position) (amount move)

steps : Int -> Int -> List Int
steps x y =
    if x < y then
        enumFromTo x y
    else
        reverse $ enumFromTo y x

allSquares : Move -> Position -> List (Int, Int)
allSquares move position =
    let
        oldX = x position

        oldY = y position

        newFace =
            joinDirection (facing position) (direction move)
        newX =
            moveX newFace (x position) (amount move)
        newY =
            moveY newFace (y position) (amount move)
    in
         drop 1 $ [ ( x, y) |
            x <- steps oldX newX,
            y <- steps oldY newY
            ]





manyMoves : List Move -> Position -> Position
manyMoves [] pos = pos
manyMoves [x] pos = moveAlong x pos
manyMoves (x::xs) pos = manyMoves xs (moveAlong x pos)

firstDupe : List (Int, Int) -> List (Int, Int) -> (Int, Int)
firstDupe [] [] = (0, 0)
firstDupe [] (x::_) = x
firstDupe (pos::positions) seen =
    if elem pos seen then
        firstDupe [] (pos::seen)
    else
        firstDupe positions (pos::seen)


visitedPlaces : List Move -> Position -> List (Int, Int) -> List (Int, Int)
visitedPlaces [] _ visitedPos = visitedPos
visitedPlaces (move::moves) start visitedPos =
    visitedPlaces moves (moveAlong move start) (visitedPos ++ (allSquares move start))


input : String
input = "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, R4"

firstStep : IO ()
firstStep =
    putStrLn $ show $ distanceBetween (manyMoves (collectMovesFromString input) Main.start) Main.start


secondStep : IO ()
secondStep =
    putStrLn
        $ show
        $ distanceBetween (Main.start)
        $ (\x => firstDupe x [])
        $ visitedPlaces (collectMovesFromString input) Main.start [(x Main.start, y Main.start)]


main : IO ()
main =
    secondStep




assertEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

assertNotEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertNotEq g e = if not (g == e)
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"


testDistance : IO ()
testDistance =
    assertEq 5 $
        distanceBetween (Pos 2 3 UUp) Main.start


moved : Position
moved =
    moveAlong (MakeMove RRight 2) $
    moveAlong (MakeMove RRight 2) $
    moveAlong (MakeMove RRight 2) Main.start

testMoves : IO ()
testMoves =
    assertEq ( - 2) $
        y moved



