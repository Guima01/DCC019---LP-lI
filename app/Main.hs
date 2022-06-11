import Data.List.Split
import Data.List 
import System.Random (randomRIO)

sumValue :: Int -> Int -> Int -> Int
sumValue r t n = if r == t then n + 1 else n

removeFirst :: [Int] -> [Int]
removeFirst (x:xs) = xs

compareCode :: [Int] -> [Int] -> Int
compareCode [] _ = 0
compareCode a b =
    if head a == head b then
        1 + compareCode (tail a) (tail b)
    else
        compareCode (tail a) (tail b)

compareCode2 :: [Int] -> [Int] -> [Bool]
compareCode2 [] _ = []
compareCode2 a b = 
    if head a == head b then
        (True:compareCode2 (tail a) (tail b))
    else
        (False:compareCode2 (tail a) (tail b))

compareElementPartial :: Int -> [Int] -> [Bool] -> [Bool]
compareElementPartial _ [] _ = []
compareElementPartial a b listBool =
    if a /= head b || head listBool == True then
        (False:compareElementPartial a (tail b) (tail listBool))
    else
        (True: listOfFalse (tail listBool))

-- comparePartialCode :: [Int] -> [Int] -> [Bool] -> [Bool]
-- comparePartialCode _ [] _ = []
-- comparePartialCode a b listBool = do
--     let bool2 = compareElementPartial (head a) b listBool
    -- let a2 = removeFirst a
    -- let bool2Adjusted = compareListBool (listBool) (bool2)
    -- show bool2Adjusted
    -- return (True)
    -- bool3 <- compareElementPartial (head a2) (b) (bool2Adjusted)
    -- return bool3


listOfFalse :: [Bool] -> [Bool]
listOfFalse []  = []
listOfFalse a =
    (False :listOfFalse (tail a))

compareListBool :: [Bool] -> [Bool] -> [Bool]
compareListBool [] [] = []
compareListBool a b =
    if head a /= head b then
        (True: compareListBool (tail a) (tail b))
    else
        (False: compareListBool (tail a) (tail b))

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs) 

countResult :: [Bool] -> Int
countResult [] = 0
countResult a =
    if head a == True then
        1 + countResult (tail a)
    else
        countResult (tail a) 

comparePartialCode :: [Int] -> [Int] -> [Bool] -> [Bool]
comparePartialCode [] generatedList listBool = listBool
comparePartialCode userList generatedList listBool = do 
    let partialListBool = compareElementPartial (head userList) (generatedList) (listBool)
    let newList = removeFirst userList
    let partialListBoolAdjusted = compareListBool (listBool) (partialListBool)
    comparePartialCode newList generatedList partialListBoolAdjusted
    -- let bool3 = compareElementPartial (head a2) (generatedList) (bool2Adjusted)
    -- let a3 = removeFirst a2
    -- let bool3Adjusted = compareListBool (bool2Adjusted) (bool3)
    -- let bool4 = compareElementPartial (head a3) (generatedList) (bool3Adjusted)
    -- let a4 = removeFirst a3
    -- let bool4Adjusted = compareListBool (bool3Adjusted) (bool4)
    -- let bool5 = compareElementPartial (head a4) (generatedList) (bool4Adjusted)
    -- let a5 = removeFirst a4
    -- let bool5Adjusted = compareListBool (bool4Adjusted) (bool5)
    -- bool5Adjusted

-- teste :: [Int] -> [Int] -> [Bool] -> [Bool]
-- teste userList generatedList listBool = do 
--     let bool2 = compareElementPartial (head userList) (generatedList) (listBool)
--     let a2 = removeFirst userList
--     let bool2Adjusted = compareListBool (listBool) (bool2)
--     let bool3 = compareElementPartial (head a2) (generatedList) (bool2Adjusted)
--     let a3 = removeFirst a2
--     let bool3Adjusted = compareListBool (bool2Adjusted) (bool3)
--     let bool4 = compareElementPartial (head a3) (generatedList) (bool3Adjusted)
--     let a4 = removeFirst a3
--     let bool4Adjusted = compareListBool (bool3Adjusted) (bool4)
--     let bool5 = compareElementPartial (head a4) (generatedList) (bool4Adjusted)
--     let a5 = removeFirst a4
--     let bool5Adjusted = compareListBool (bool4Adjusted) (bool5)
--     bool5Adjusted





main :: IO ()
main = do
    let generatedList = [3,6,7,8]
    putStrLn "Please enter the code"
    line <- getLine  
    let userList = map (read::String->Int) (splitOn  " " line)
    let resultCompleto = compareCode2 userList generatedList
    let completo = countResult resultCompleto
    let resultParcial= comparePartialCode (userList) (generatedList) (resultCompleto)


    let parcial = countResult resultParcial
    
    print(show completo ++ " Completo, " ++ show (parcial-completo) ++ " Parcial")
    