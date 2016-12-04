module DayFour

import Data.String as String
import Debug.Trace as Debug

--aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
--a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
--not-a-real-room-404[oarel] is a real room.
--totally-real-room-200[decoy] is not.

export
record Room where
    constructor MakeRoom
    name : List String
    checksum : String
    sectorId : Int

parseSectorId : String -> Maybe (Int, String)
parseSectorId str =
    case Strings.split (== '[') str of
        [] => Nothing
        (id_::checksum::xs) =>
            map (\actualId => (actualId, reverse $ strTail $ reverse $ checksum))
                $ String.parseInteger $ id_
        _ => Nothing

splitNameAndRest : String -> (List String, List String)
splitNameAndRest str =
    (filter (not . elem '[' . unpack) splitted, filter (elem '[' . unpack) splitted)
    where
        splitted = split (== '-') str


uniqueLetters : List Char -> List Char
uniqueLetters [] = []
uniqueLetters (x::xs) =
    if elem x restLetters then
        restLetters
    else
        x::restLetters
    where
        restLetters = uniqueLetters xs

allLetters : List Char
allLetters =
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'
    , 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'
    , 'q', 'r', 's', 't', 'u', 'v', 'w', 'x'
    , 'y', 'z'
    ]

lettersWithCount : List Char -> List Char -> List (Char, Nat)
lettersWithCount _ [] = []
lettersWithCount [] _ = []
lettersWithCount word (x::xs) =
    if amount == Z then
        lettersWithCount word xs
    else
        (x, amount) :: (lettersWithCount word xs)
    where
        amount = List.length $ findIndices (== x) word

sortByLetters : (Char, Nat) -> (Char, Nat) -> Ordering
sortByLetters (x, y) (a, b) =
    if y == b then
        compare x a
    else
        compare b y

roomChecksum : Room -> String
roomChecksum (MakeRoom name checksum id_) =
    pack
        $ map fst
        $ List.take 5
        $ sortBy sortByLetters
        $ lettersWithCount nameAllJoined allLetters
    where
        nameAllJoined = unpack $ concat name

export
isValidRoom : Room -> Bool
isValidRoom room =
    (roomChecksum room == checksum room)

export
Show Room where
    show room@(MakeRoom name checksum sectorId) =
        show (concat $ intersperse "-" name) ++
            ", id = " ++ show sectorId ++
            ", checksum = '" ++ show checksum ++
            "', isValidRoom = " ++ (show (isValidRoom room))

export
parseRoom : String -> Maybe Room
parseRoom str =
    case splitNameAndRest str of
        ([], []) => Nothing
        (name, [rest]) =>
            case parseSectorId rest of
                Nothing => Nothing
                Just (id_, checksum) =>
                    Just $ MakeRoom name checksum id_
        thing => Debug.trace (show thing) $ Nothing

export
parseRooms : String -> List Room
parseRooms =
    mapMaybe parseRoom . lines

export
collectRealRooms : String -> List Room
collectRealRooms =
    filter isValidRoom . parseRooms


export
sumIds : List Room -> Int
sumIds =
    sum . map (sectorId)

unencryptLetter : Int -> Char -> Char
unencryptLetter id_ letter =
    chr
        $ (+ 96)
        $ mod (id_ + (ord letter - 96)) 26

unencryptWord : Int -> String -> String
unencryptWord id_ =
    pack . map (unencryptLetter id_) . unpack

export
unencryptName : Room -> String
unencryptName (MakeRoom name checksum id_) =
    unwords $ map (unencryptWord id_) name
