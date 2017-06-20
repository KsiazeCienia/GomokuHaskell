{-# LANGUAGE InstanceSigs #-}

import Data.List
import System.Random
import System.IO
import System.Environment
import System.IO.Error

aiVSaiLoop :: Board -> Figure -> IO()
aiVSaiLoop board f = do
    gen <- getStdGen
    newStdGen
    let (x, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
    gen <- getStdGen
    newStdGen
    let (y, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
    if (figure (cells board !! x !! y) == Empty) then do
        let board2 = insertFigure board (x) (y) f
        putStrLn $ show board2
        putStrLn "------------------------------------------"
        if(isWinner board2 f x y 0 == 5) then do
            end board2 f
        else do
            changeAI board2 f
    else do
        aiVSaiLoop board f

changeAI :: Board -> Figure -> IO()
changeAI board f = do
    if(f == Circle) then do
        aiVSaiLoop board Cross
    else do
        aiVSaiLoop board Circle


aiVSai :: IO()
aiVSai = do
    aiVSaiLoop board Cross

userVSai :: IO()
userVSai = do
    userVSaiLoop board Cross

userVSaiLoop :: Board -> Figure -> IO()
userVSaiLoop board f = do
    if (f == Circle) then do
        gen <- getStdGen
        newStdGen
        let (x, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
        gen <- getStdGen
        newStdGen
        let (y, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
        if (figure (cells board !! x !! y) == Empty) then do
            let board2 = insertFigure board (x) (y) f
            putStrLn $ show board2
            putStrLn "------------------------------------------"
            if(isWinner board2 f x y 0 == 5) then do
                end board2 f
            else do
                changeAiPlayer board2 f
        else do
            aiVSaiLoop board f
    else do
        putStrLn "Podaj numer kolumny"
        x <- getLine
        putStrLn "Podaj numer wiersza"
        y <- getLine
        if (figure (cells board !! (read x::Int) !! (read y::Int)) == Empty) then do
            let board2 = insertFigure board (read x::Int) (read y::Int) f
            putStrLn $ show board2
            putStrLn "------------------------------------------"
            if(isWinner board2 f (read x::Int) (read y::Int) 0 == 5) then do
                end board2 f
            else do
                changeAiPlayer board2 f
        else do
            putStrLn "Podałeś niepoprawne dane spróbuj ponownie"
            aiVSaiLoop board f

changeAiPlayer :: Board -> Figure -> IO()
changeAiPlayer board f = do
    if(f == Circle) then do
        userVSaiLoop board Cross
    else do
        userVSaiLoop board Circle


userVSuser :: IO()
userVSuser = do
    userVSuserLoop board Cross

userVSuserLoop :: Board -> Figure -> IO()
userVSuserLoop board f = do
    if (f == Circle) then do
        putStrLn "Teraz kolej o"
    else do
        putStrLn "Teraz kolej x"
    putStrLn "Podaj numer kolumny"
    x <- getLine
    putStrLn "Podaj numer wiersza"
    y <- getLine
    if (figure (cells board !! (read x::Int) !! (read y::Int)) == Empty) then do
        let board2 = insertFigure board (read x::Int) (read y::Int) f
        putStrLn $ show board2
        putStrLn "------------------------------------------"
        if(isWinner board2 f (read x::Int) (read y::Int) 0 == 5) then do
            end board2 f
        else do
            changePlayer board2 f
    else do
        putStrLn "Podałeś niepoprawne dane spróbuj ponownie"
        aiVSaiLoop board f

changePlayer :: Board -> Figure -> IO()
changePlayer board f = do
    if(f == Circle) then do
        userVSuserLoop board Cross
    else do
        userVSuserLoop board Circle



main :: IO()
main = do
    putStrLn "Wciśnij numer aby wybrać"
    putStrLn "1. ai vs ai"
    putStrLn "2. user vs ai"
    putStrLn "3. user vs user"
    choice <- getLine
    case choice of
        "1" -> aiVSai
        "2" -> userVSai
        "3" -> userVSuser


end:: Board -> Figure -> IO()
end board f = do
    putStrLn $ show board
    putStr "Wygrał "
    putStrLn $ show f

data Figure = Cross | Circle | Empty deriving Eq

data Point = Point {x :: Int, y :: Int, figure :: Figure}

instance Show Point where
    show (Point x y figure) = show figure

instance Eq Point where
    (Point _ _ x) == (Point _ _ y) = x == y

instance Show Figure where
    show Cross = "x"
    show Circle = "o"
    show Empty = "_"

data Board = Board {size :: Int, cells :: [[Point]]}

instance Show Board where
    show (Board _ rows) = intercalate "\n" $ map show rows

makeBoard :: Int -> Int-> [[Point]] -> [[Point]]
makeBoard x y list
    | x == -1 = list
    | otherwise = makeBoard (x - 1) y (b : list)
    where b = makeColumns x y []

makeColumns :: Int -> Int -> [Point] -> [Point]
makeColumns x y list
    | y == -1 = list
    | otherwise = makeColumns x (y - 1) (b : list)
    where b = Point x y Empty

insertFigure :: Board -> Int -> Int -> Figure -> Board
insertFigure board x y f
    | x > ((size board) - 1) || y > ((size board) - 1) || x < 0 || y < 0 = board
    | figure (cells board !! x !! y) == Empty = Board (size board) (insertPoint board x y f)
    | otherwise = board

insertPoint :: Board -> Int -> Int -> Figure -> [[Point]]
insertPoint board x y figure =
    let (a, b) = splitAt x (cells board)
        in let c = [changeColumn (head b) x y figure]
            in a ++ (c) ++ (tail b)

changeColumn :: [Point] -> Int -> Int -> Figure -> [Point]
changeColumn column x y figure =
    let(a, b) = splitAt y column
        in a ++ [Point x y figure] ++ tail b

checkUp :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkUp board f x y score l
    | x < 0  = checkDown board f (x + l + 1) y score
    | figure (cells board !! x !! y) == f = checkUp board f (x - 1) y (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkDown board f (x + l + 1) y score

checkDown :: Board -> Figure -> Int -> Int -> Int -> Int
checkDown board f x y score
     | x > ((size board) - 1) = score
     | figure ( cells board !! x !! y ) == f = checkDown board f (x + 1) y (score + 1)
     | score == 1 = checkLeft board f (x - 1) (y - 1) score 1
     | otherwise = score

checkLeft :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkLeft board f x y score l
    | y < 0 = checkRight board f x (y + l + 1) score 1
    | figure (cells board !! x !! y ) == f = checkLeft board f x (y - 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkRight board f x (y + l + 1) score 1

checkRight :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkRight board f x y score l
    | y > ((size board) - 1) = score
    | figure (cells board !! x !! y ) == f = checkRight board f x (y + 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkUpLeft board f (x - 1) (y - l - 1) 1 1

checkUpLeft :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkUpLeft board f x y score l
    | x < 0 || y < 0 = checkDownRight board f (x + l + 1) (y + l + 1) score 1
    | figure (cells board !! x !! y ) == f = checkUpLeft board f (x - 1) (y - 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkDownRight board f (x + l + 1) (y + l + 1) 1 1

checkDownRight :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkDownRight board f x y score l
    | x > ((size board) - 1) || y > ((size board) - 1) = checkUpRight board f (x - l - 1) y 1 1
    | figure (cells board !! x !! y ) == f = checkDownRight board f (x + 1) (y + 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkUpRight board f (x - l - 1) y 1 1

checkUpRight :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkUpRight board f x y score l
    | x < 0 || y > ((size board) - 1) = checkDownLeft board f (x + l + 1) (y - l - 1) score 1
    | figure (cells board !! x !! y ) == f = checkUpRight board f (x - 1) (y + 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = checkDownLeft board f (x + l + 1) (y - l - 1) 1 1

checkDownLeft :: Board -> Figure -> Int -> Int -> Int -> Int -> Int
checkDownLeft board f x y score l
    | y < 0 || x > ((size board) - 1) = score
    | figure (cells board !! x !! y) == f = checkDownLeft board f (x + 1) (y - 1) (score + 1) (l + 1)
    | otherwise && score == 5 = score
    | otherwise = score

isWinner :: Board -> Figure -> Int -> Int -> Int -> Int
isWinner board f x y score
    | x == ((size board) - 1) || y == ((size board) - 1) = score
    | figure (cells board !! x !! y) == f = checkUp board f (x - 1) y (score + 1) 1
    | otherwise = score

board = Board 19 (makeBoard 18 18 [])
board5 = insertFigure board 3 1 Circle
point5 = Point 3 1 Circle
point1 = Point 1 1 Circle
board1 = insertFigure board 1 1 Circle
point2 = Point 4 4 Circle
board2 = insertFigure board 4 4 Circle
point3 = Point 1 4 Circle
board3 = insertFigure board 1 4 Circle
point4 = Point 4 1 Circle
board4 = insertFigure board 4 1 Circle
