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
    | x == 0 = list
    | otherwise = makeBoard (x - 1) y (b : list)
    where b = makeColumns x y []

makeColumns :: Int -> Int -> [Point] -> [Point]
makeColumns x y list
    | y == 0 = list
    | otherwise = makeColumns x (y - 1) (b : list)
    where b = Point x y Empty

insertFigure :: Board -> Int -> Int -> Figure -> Board
insertFigure board x y figure
    | figure == Empty = board
    | (x <= size board) && (x > 0) && (y <= size board) && (y > 0) = Board (size board) (insertPoint board x y figure)
    | otherwise = board

insertPoint :: Board -> Int -> Int -> Figure -> [[Point]]
insertPoint board x y figure =
    let (h, t) = splitAt (x - 1) (cells board)
        in let column = head t
            in let newColumn = [changeColumn column x y figure]
                in (h ++ newColumn ++ (tail t))

changeColumn :: [Point] -> Int -> Int -> Figure -> [Point]
changeColumn column x y figure =
    let (h, t) = splitAt (y - 1) column
            in (h ++ [Point x y figure] ++ (tail t))

loop :: Board -> Figure -> IO()
loop board figure1 = do
 gen <-getStdGen
 newStdGen
 let (number,_) = randomR (1,(size board) - 1) gen :: (Int, StdGen)
 gen <-getStdGen
 newStdGen
 let (number2,_) = randomR (1,(size board) - 1) gen :: (Int, StdGen)
 if( figure (cells board !! number !! number2) == Empty) then do
  let board2 = insertFigure board (number) (number2) figure1
  putStrLn $ show board2
  checkWin board2 figure1
 else do
  loop board figure1

checkWin:: Board -> Figure->IO()
checkWin board figure = do
 if(figure==Circle) then do
  let figure = Cross
  loop board figure
 else do
  let figure = Circle
  loop board figure



board = Board 19 (makeBoard 18 18 [])

main :: IO()
main = do
  loop board Cross


end:: Board -> Figure -> IO()
end board figure = do
putStrLn $ show board
putStr "Wygrał "
putStrLn $ show figure
