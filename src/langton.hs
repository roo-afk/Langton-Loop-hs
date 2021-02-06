

import Data.Char
import Data.Array
import System.Console.ANSI
import Control.Concurrent (threadDelay)
import qualified Data.HashMap as M

zipWithIndices :: [[a]] -> [((Int, Int), a)]
-- concat is to flatten [[]] into [] of elements and their indices
zipWithIndices = concat . zipWith (\i xs -> zipWith (\j a -> ((i, j), a)) [0..] xs) [0..] 

startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]

characters = zip [0..7] (' ':['1'..'7'])

rows :: Int
rows = 70

cols :: Int
cols = 85


rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a

row a j = [a ! (j,k) | k <- colIndices a]

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]






changeColorMode c = 
    case c of
     ' ' -> setSGR [ SetColor Foreground Vivid Black ]
     '1' -> setSGR [ SetColor Foreground Vivid Blue ]
     '2' -> setSGR [ SetColor Foreground Vivid Red ]
     '3' -> setSGR [ SetColor Foreground Vivid Green ]
     '4' -> setSGR [ SetColor Foreground Vivid Yellow ]
     '5' -> setSGR [ SetColor Foreground Vivid Magenta ]
     '6' -> setSGR [ SetColor Foreground Vivid White]
     '7' -> setSGR [ SetColor Foreground Vivid Cyan ]

printArrayFat = sequence_ . fmap putStrFatLn . arrayToLulz

putStrFatLn cs = do
  sequence_ [changeColorMode c >> putChar c >> putChar ' '| c <- cs]
  putChar '\n'


emptyArray item m n = array b [(i,item) | i <- range b]
    where b = ((0,0),(m-1,n-1))



intToDigitArray :: Array (Int, Int) Int -> Array (Int, Int) Char
intToDigitArray = fmap intToDigit


neighbors :: Array (Int, Int) Int -> (Int, Int) -> ((Int, Int), (Int, Int)) -> String
neighbors a (j,k) ((rMin, cMin), (rMax, cMax)) = 
  let j' = j - 1
      j'' = j + 1
      k' = k - 1
      k'' = k + 1 
    in  
     if j' < rMin || j'' > rMax || k' < cMin || k'' > cMax
        then [] -- dummy
        else map intToDigit [a ! (j, k), a ! (j', k), a ! (j, k''), a ! (j'', k), a ! (j, k')]

neighborsArray a = array bounds' [ (i, neighbors a (j,k) bounds')| i@(j,k) <- indices a] where bounds' = bounds a

zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int,Int) b -> Array (Int,Int) c
zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) (b ! (j,k)) ) | i@(j,k) <- indices a]




rotate (x:xs) = x : y : xs'
  where 
    (xs', [y]) = splitAt 3 xs


rotations = take 4 . iterate rotate

makePair xs = let (xs', [x]) = splitAt 5 xs in (xs', x)


-- could also create the rules table by using 5d array

makePairs :: [String] -> [(String, Char)]
makePairs = concatMap f
   where 
       f str = 
         let (xs, x) = makePair str
           in map (\xs' -> (xs', x)) $ rotations xs




intToGameArray :: Array (Int, Int) Int -> Array (Int, Int) Char
intToGameArray = fmap f
  where 
       f x = let Just y = lookup x characters in y

gameUpdate :: Array (Int, Int) Int -> M.Map String Char -> Array (Int, Int) Int
gameUpdate a rules = zipWithArray f a (neighborsArray a)
  where 
    f x str = 
      case M.lookup str rules of
       Nothing   -> x
       (Just x') -> digitToInt x' 


gameLoop :: Array (Int, Int) Int -> M.Map String Char -> IO ()
gameLoop a rules = 
  do 
    setCursorPosition 0 0
    printArrayFat $ intToGameArray a
    threadDelay 10000
    gameLoop (gameUpdate a rules) rules



main :: IO ()
main = do
  hideCursor
  contents <- readFile "langton-table.txt"
  let rules = M.fromList $ makePairs $ lines contents
  gameLoop c rules
   where 
     b = emptyArray 0 rows cols
     offsetRows = quot rows 2
     offsetCols = quot cols 6
     as' = map (\((x, y), z) -> ((x + offsetRows, y + offsetCols), digitToInt z)) $ zipWithIndices startLoop
     c = b // as' 



