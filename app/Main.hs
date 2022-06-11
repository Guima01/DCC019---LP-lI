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

listOfFalse :: [a] -> [Bool]
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

clearList :: [Int] -> [Bool] -> [Int]
clearList [] [] = []
clearList listInt listBool = do
    if (head listBool) == True then
        clearList (tail listInt) (tail listBool)
    else 
        ((head listInt) : (clearList (tail listInt) (tail listBool)))




-- Necessário mudar a forma de verificar a completude e parcialidade da comparação
-- Solução: Remover os valores ja comparados das listas do usuário e randomicas
masterMind :: [Int] -> Int -> IO ()
masterMind _  4 =  print("Fim")
masterMind generatedList value = do 
    putStrLn "Please enter the code"
    line <- getLine  
    let userList = map (read::String->Int) (splitOn  " " line)
    let resultCompleto = compareCode2 userList generatedList
    let newUserList = clearList userList resultCompleto
    let newGeneratedList = clearList generatedList resultCompleto
    let parcialBool = listOfFalse (newGeneratedList)
    let completo = countResult resultCompleto
    let resultParcial= comparePartialCode (newUserList) (newGeneratedList) (parcialBool)
    let parcial = countResult resultParcial
    print(show completo ++ " Completo, " ++ show (parcial) ++ " Parcial")
    masterMind generatedList completo

main :: IO ()
main = do
    generatedList <- randomList 4
    masterMind generatedList 0


    