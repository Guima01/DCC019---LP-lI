import Data.List.Split
import Data.List 
import System.Random (randomRIO)

sumValue :: Int -> Int -> Int -> Int
sumValue r t n = if r == t then n + 1 else n

compareCode :: [Int] -> [Int] -> Int
compareCode [] _ = 0
compareCode a b =
    if head a == head b then
        1 + compareCode (tail a) (tail b)
    else
        compareCode (tail a) (tail b)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 



main :: IO ()
main = do
    putStrLn "Please enter the code"
    line <- getLine  
    let userList = map (read::String->Int) (splitOn  " " line)
    generatedList <- randomList 4
    let completo = compareCode userList generatedList
    print(show completo ++ " Completo, " ++ show 0 ++ " Parcial")
    