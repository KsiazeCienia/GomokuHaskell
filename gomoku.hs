{-# LANGUAGE InstanceSigs #-}

import Data.List
import System.Random

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

isWinner :: Board -> Point -> Bool
--isWinner board point = ((checkUp board point 1) || (checkLeft board point 1) || (checkUpRight board point 1) || (checkUpLeft board point 1))
isWinner board point = (checkUp board point 1)

checkUp :: Board -> Point -> Int -> Bool
checkUp board point result
    | result == 5 = True
    | (((x point) - 1) == 0) && (figure (cells board !! 0 !! (y point)) == (figure point)) = checkDown board (cells board !! ((x point)  + result) !! (y point)) (result + 1)
    | (((x point) - 1) == 0) || (figure (cells board !! (x point) !! (y point)) /= (figure point)) = checkDown board (cells board !! ((x point)  + result) !! (y point)) result
    | (((x point) - 1) < 0) = checkDown board (cells board !! ((x point)  + result +) !! (y point)) result
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkUp board (cells board !! ((x point) - 1) !! (y point)) (result + 1)
    | otherwise = False

checkDown :: Board -> Point -> Int -> Bool
checkDown board point result
    | result == 5 = True
    | (((x point) + 1) == (size board)) && ((figure (cells board !! (size board) !! (y point))) == (figure point)) = checkDown board (cells board !! ((x point) + 1) !! (y point)) (result + 1)
    | (((x point) + 1) == (size board)) = checkDown board (cells board !! ((x point) + 1) !! (y point)) result
    | (((x point) + 1) > (size board)) && (result < 5) = False
    | figure (cells board !! (x point)  !! (y point)) == (figure point) = checkDown board (cells board !! ((x point) + 1) !! (y point)) (result + 1)
    | otherwise = False

checkLeft :: Board -> Point -> Int -> Bool
checkLeft board point result
    | result == 5 = True
    | ((y point) > (size board)) && ((y point) < 1) = checkRight board (cells board !! (x point)  !! ((y point) + result)) result
    | figure (cells board !! (x point)  !! (y point)) == (figure point) = checkLeft board (cells board !! (x point) !! ((y point) - 1)) (result + 1)
    | otherwise = False

checkRight :: Board -> Point -> Int -> Bool
checkRight board point result
    | result == 5 = True
    | ((y point) > (size board)) && ((y point) < 1) && (result < 5) = False
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkRight board (cells board !! (x point) !! ((y point) + 1)) (result + 1)
    | otherwise = False

checkUpRight :: Board -> Point -> Int -> Bool
checkUpRight board point result
    | result == 5 = True
    | ((x point) > (size board)) && ((x point) < 1) && ((y point) > (size board)) && ((y point) < 1) = checkDownLeft board (cells board !! ((x point) - result) !! ((y point) - result)) result
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkUpRight board (cells board !! ((x point) - 1) !! ((y point) + 1)) (result + 1)
    | otherwise = False

checkDownLeft :: Board -> Point -> Int -> Bool
checkDownLeft board point result
    | result == 5 = True
    | ((x point) > (size board)) && ((x point) < 1) && ((y point) > (size board)) && ((y point) < 1) && (result < 5) = False
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkDownLeft board (cells board !! ((x point) + 1) !! ((y point) - 1)) (result + 1)
    | otherwise = False

checkUpLeft :: Board -> Point -> Int -> Bool
checkUpLeft board point result
    | result == 5 = True
    | ((x point) > (size board)) && ((x point) < 1) && ((y point) > (size board)) && ((y point) < 1) = checkDownRight board (cells board !! ((x point) - result) !! ((y point) + result)) result
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkUpLeft board (cells board !! ((x point) - 1) !! ((y point) - 1)) (result + 1)
    | otherwise = False

checkDownRight :: Board -> Point -> Int -> Bool
checkDownRight board point result
    | result == 5 = True
    | ((x point) > (size board)) && ((x point) < 1) && ((y point) > (size board)) && ((y point) < 1) && (result < 5) = False
    | figure (cells board !! (x point) !! (y point)) == (figure point) = checkDownRight board (cells board !! ((x point) + 1) !! ((y point) + 1)) (result + 1)
    | otherwise = False

board = Board 5 (makeBoard 4 4 [])
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


loop :: Board -> Figure -> IO()
loop board figure1 = do
    gen <- getStdGen
    newStdGen
    let (number, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
    gen <- getStdGen
    newStdGen
    let (number2, _) = randomR (0, ((size board) - 1)) gen :: (Int, StdGen)
    if (figure (cells board !! number !! number2) == Empty) then do
        let board2 = insertFigure board (number) (number2) figure1
        putStrLn $ show board2
        putStrLn "--------------"
        if ((isWinner board2 (Point number number2 figure1)) == True) then do
            end board2 figure1
        else do
            checkWin board2 figure1
    else do
        loop board figure1

checkWin:: Board -> Figure -> IO()
checkWin board figure = do
    if(figure == Circle) then do
        let figure = Cross
        loop board figure
    else do
        let figure = Circle
        loop board figure


main :: IO()
main = do
  loop board Cross


end:: Board -> Figure -> IO()
end board figure = do
    putStrLn $ show board
    putStr "Wygrał "
    putStrLn $ show figure
