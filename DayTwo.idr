
import Data.Vect

record Keypad where
    constructor MakeKeypad
    limitX : Int
    limitY : Int


intoGroups : Nat -> List Int -> List (List Int)
intoGroups Z _ = []
intoGroups _ [] = []
intoGroups n xs = (List.take n xs) :: (intoGroups n $ drop n xs)


keypadToGrid : Keypad -> List (List Int)
keypadToGrid (MakeKeypad limitX limitY) =
    intoGroups (toNat limitX) [1..(limitX * limitY)]


record Position where
    constructor Pos
    x : Int
    y : Int

Show Position where
    show (Pos x y) =
        "x = " ++ show x ++ ", y = " ++ show y

positionToGridPoint : Keypad -> Position -> Int
positionToGridPoint (MakeKeypad limitX _) (Pos x y) =
   (mod x limitX) + (y * limitX) + 1

data Move = UUp | DDown | LLeft | RRight

implementation Show Move where
    show UUp = "Up"
    show DDown = "Down"
    show LLeft = "Left"
    show RRight = "Right"

movesFromListChar : List Char -> List Move
movesFromListChar [] = []
movesFromListChar ('R'::xs) = RRight :: movesFromListChar xs
movesFromListChar ('L'::xs) = LLeft :: movesFromListChar xs
movesFromListChar ('U'::xs) = UUp :: movesFromListChar xs
movesFromListChar ('D'::xs) = DDown :: movesFromListChar xs
movesFromListChar (_::xs) = movesFromListChar xs

movesFromString : String -> List Move
movesFromString =
    movesFromListChar . unpack


movesFromLines : String -> List (List Move)
movesFromLines =
    map movesFromString . lines


moveAlong : Move -> Position -> Position
moveAlong UUp (Pos x y) = Pos x (y - 1)
moveAlong DDown (Pos x y) = Pos x (y + 1)
moveAlong RRight (Pos x y) = Pos (x + 1) y
moveAlong LLeft (Pos x y) = Pos (x - 1) y


moveAllAlong : List Move -> Position -> Position
moveAllAlong moves pos =
    foldl (flip moveAlong) pos moves

inBounds : Keypad -> Position -> Bool
inBounds (MakeKeypad limitX limitY) (Pos x y) =
    if x < 0 || x >= limitX || y < 0 || y >= limitY then
        False
    else
        True

moveAllAlongRestricted : Keypad -> List Move -> Position -> Position
moveAllAlongRestricted _ [] pos = pos
moveAllAlongRestricted keypad (move::xs) pos =
    moveAllAlongRestricted keypad xs
        $
            if inBounds keypad nextPos then
                nextPos
            else
                pos
    where
        nextPos = moveAlong move pos


collectGridPoints : Keypad -> List (List Move) -> Position -> List Int
collectGridPoints _ [] _ = []
collectGridPoints keypad (moves::xs) pos =
    positionToGridPoint keypad nextPos :: collectGridPoints keypad xs nextPos
        where
            nextPos = moveAllAlongRestricted keypad moves pos


defaultKeypad : Keypad
defaultKeypad = MakeKeypad 3 3

defaultPosition : Position
defaultPosition = Pos 1 1

exampleInstructions : String
exampleInstructions =
"""ULL
RRDDD
LURDL
UUUUD"""


runExample : IO ()
runExample =
    putStrLn
        $ show
        --$ movesFromLines exampleInstructions
        $ collectGridPoints defaultKeypad (movesFromLines exampleInstructions) defaultPosition


questionInstructions : String
questionInstructions =
"""ULUULLUULUUUUDURUUULLDLDDRDRDULULRULLRLULRUDRRLDDLRULLLDRDRRDDLLLLDURUURDUDUUURDRLRLLURUDRDULURRUDLRDRRLLRDULLDURURLLLULLRLUDDLRRURRLDULRDDULDLRLURDUDRLLRUDDRLRDLLDDUURLRUDDURRLRURLDDDURRDLLDUUDLLLDUDURLUDURLRDLURURRLRLDDRURRLRRDURLURURRRULRRDLDDDDLLRDLDDDRDDRLUUDDLDUURUULDLUULUURRDRLDDDULRRRRULULLRLLDDUDRLRRLLLLLDRULURLLDULULLUULDDRURUDULDRDRRURLDRDDLULRDDRDLRLUDLLLDUDULUUUUDRDRURDDULLRDRLRRURLRDLRRRRUDDLRDDUDLDLUUDLDDRRRDRLLRLUURUDRUUULUDDDLDUULULLRUDULULLLDRLDDLLUUDRDDDDRUDURDRRUUDDLRRRRURLURLD
LDLUDDLLDDRLLDLDRDDDDDUURUDDDUURLRLRLDULLLDLUDDDULLDUDLRUUDDLUULLDRLDDUDLUDDLURRRLDUURDDRULLURLLRLLUUDRLDDDLDLDRDUDLRDURULDLDRRDRLDLUURRRRLUDDULDULUUUDULDDRLLDDRRUULURRUURRLDUUUDDDDRUURUDRRRDDDDLRLURRRRUUDDDULRRURRDLULRURDDRDRLUDLURDDRDURRUURDUDUDRRDDURRRDURDLUUUURRUDULLDDRLLLURLDUDRRLDDLULUDUDDDDUDLUUULUURUDRURUUDUUURRLDUUDRDRURLLDLLLLLRLLUDURDRRLULRRDDDRLDRDDURLRDULULLDDURURLRRDRULDULUUUURLDURUDUDUDDLUDRRDURULRDULLLDRRDLDLUDURDULULLDDURDDUDRUUUDUDRLDUURDUUUDUURURUDRULRURLDLRDDURDLUU
DDLDRLLDRRDRRLLUUURDDULRDUDRDRUDULURLLDDLRRRUDRDLDLURRRULUDRDLULLULLDUUDRLRUDDLRRURRUULRLDLLLDLRLLLURLLLURLLRDDLULLDUURLURDLLDLDUDLDRUUUDDLLDRRRRRUDRURUURRRDRUURDRDDRLDUUULUDUDRUDLLLLDRDRURRRDUUURLDLRLRDDDRLUDULDRLLULRDLDURDLDURUUDDULLULRDDRLRUURLLLURDRUURUUDUUULRDUDDRDURRRDUUDRRRUDRDLRURDLLDDDURLLRRDDDDLDULULDRLDRULDDLRRRLUDLLLLUDURRRUURUUULRRLDUURDLURRLRLLRDLRDDRDDLRDLULRUUUDDDUDRRURDDURURDDUDLURUUURUUUUDURDDLDRDULDRLDRLLRLRRRLDRLLDDRDLDLUDDLUDLULDLLDRDLLRDULDUDDULRRRUUDULDULRRURLRDRUDLDUDLURRRDDULRDDRULDLUUDDLRDUURDRDR
URDURRRRUURULDLRUUDURDLLDUULULDURUDULLUDULRUDUUURLDRRULRRLLRDUURDDDLRDDRULUUURRRRDLLDLRLRULDLRRRRUDULDDURDLDUUULDURLLUDLURULLURRRDRLLDRRDULUDDURLDULLDURLUDUULRRLLURURLDLLLURDUDRLDDDRDULLUDDRLDDRRRLDULLLLDUURULUDDDURUULUUUDURUDURDURULLLDRULULDRRLDRLDLRLRUDUDURRLURLRUUDRRDULULDLLDRDRRRDUDUURLDULLLURRDLUDDLDDRDDUDLDDRRRUDRULLURDDULRLDUDDDRULURLLUDLLRLRRDRDRRURUUUURDLUURRDULLRDLDLRDDRDRLLLRRDDLDDDDLUDLRLULRRDDRDLDLUUUDLDURURLULLLDDDULURLRRURLDDRDDLD
UDUULLRLUDLLUULRURRUUDDLLLDUURRURURDDRDLRRURLLRURLDDDRRDDUDRLLDRRUDRDRDDRURLULDDLDLRRUDDULLRLDDLRURLUURUURURDLDUDRLUUURRRLUURUDUDUUDDLDULUULRLDLLURLDRUDRLLRULURDLDDLLULLDRRUUDDLRRRUDDLRDRRRULDRDDRRULLLUDRUULURDUDRDLRRLDLRLRLDDULRRLULUUDDULDUDDULRRURLRDRDURUDDDLLRLDRDRULDDLLRLLRDUDDDDDDRLRLLDURUULDUUUDRURRLLRLDDDDRDRDUURRURDRDLLLUDDRDRRRDLUDLUUDRULURDLLLLLRDUDLLRULUULRLULRURULRLRRULUURLUDLDLLUURDLLULLLDDLRUDDRULRDLULRUURLDRULRRLULRLRULRDLURLLRURULRDRDLRRLRRDRUUURURULLLDDUURLDUDLLRRLRLRULLDUUUULDDUUU"""


runQuestion : IO ()
runQuestion =
    putStrLn
        $ show
        $ collectGridPoints defaultKeypad (movesFromLines questionInstructions) defaultPosition


-- the second answer stuff

data ComplexGrid : List (List (Maybe String))

secondQuestionGrid : List (List (Maybe String))
secondQuestionGrid =
    [ [ Nothing, Nothing, Just "1", Nothing, Nothing ]
    , [ Nothing, Just "2", Just "3", Just "4", Nothing ]
    , [ Just "5", Just "6", Just "7", Just "8", Just "9" ]
    , [ Nothing, Just "A", Just "B", Just "C", Nothing ]
    , [ Nothing, Nothing, Just "D", Nothing, Nothing ]
    ]


validIndexes : List (Int, Int)
validIndexes =
    [ (2, 0)
    , (1, 1), (2, 1), (3, 1)
    , (0, 2), (1, 2), (2, 2), (3, 2), (4, 2)
    , (1, 3), (2, 3), (3, 3)
    , (2, 4)
    ]

isValidComplexMove : List (Int, Int) -> Position -> Bool
isValidComplexMove indexes (Pos x y) =
    elem (x, y) indexes


moveAllAlongComplex : List (Int, Int) -> List Move -> Position -> Position
moveAllAlongComplex _ [] pos = pos
moveAllAlongComplex indexes (move::xs) pos =
    moveAllAlongComplex indexes xs
        $
            if isValidComplexMove indexes nextPos then
                nextPos
            else
                pos
    where
        nextPos = moveAlong move pos

positionToComplexGridPoint : List (List (Maybe a)) -> Position -> Maybe a
positionToComplexGridPoint xs (Pos x y) =
    element
    where
        row : Maybe (List (Maybe a))
        row = index' (toNat y) xs

        element : Maybe a
        element =
            case row of
                Nothing => Nothing
                Just row' =>
                    case index' (toNat x) row' of
                        Nothing => Nothing
                        Just v => v


collectComplexGridPoints : List (List (Maybe a)) -> List (List Move) -> Position -> List (Maybe a)
collectComplexGridPoints _ [] _ = []
collectComplexGridPoints grid (moves::xs) pos =
    positionToComplexGridPoint grid nextPos :: collectComplexGridPoints grid xs nextPos
        where
            nextPos = moveAllAlongComplex validIndexes moves pos


runComplexExample : IO ()
runComplexExample =
    putStrLn
        $ show
        $ collectComplexGridPoints secondQuestionGrid (movesFromLines exampleInstructions) (Pos 0 2)


runComplexQuestion : IO ()
runComplexQuestion =
    putStrLn
        $ show
        $ collectComplexGridPoints secondQuestionGrid (movesFromLines questionInstructions) (Pos 0 2)
