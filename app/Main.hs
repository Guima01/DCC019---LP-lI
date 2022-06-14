import Data.List.Split
import Data.List 
import System.Random (randomRIO)

removeFirst :: [Int] -> [Int]
removeFirst (x:xs) = xs

compareCode :: [Int] -> [Int] -> [Bool]
compareCode [] _ = []
compareCode a b = 
    if head a == head b then
        (True:compareCode (tail a) (tail b))
    else
        (False:compareCode (tail a) (tail b))

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

compareElementPartial :: Int -> [Int] -> [Bool] -> [Bool]
compareElementPartial _ [] _ = []
compareElementPartial a b listBool =
    if a /= head b || head listBool == True then
        (False:compareElementPartial a (tail b) (tail listBool))
    else
        (True: listOfFalse (tail listBool))

comparePartialCode :: [Int] -> [Int] -> [Bool] -> [Bool]
comparePartialCode [] generatedList listBool = listBool
comparePartialCode userList generatedList listBool = do 
    let partialListBool = compareElementPartial (head userList) (generatedList) (listBool)
    let newList = removeFirst userList
    let partialListBoolAdjusted = compareListBool (listBool) (partialListBool)
    comparePartialCode newList generatedList partialListBoolAdjusted

clearList :: [Int] -> [Bool] -> [Int]
clearList [] [] = []
clearList listInt listBool =
    if (head listBool) == True then
        clearList (tail listInt) (tail listBool)
    else 
        ((head listInt) : (clearList (tail listInt) (tail listBool)))




criptoGame :: [Int] -> Int -> IO ()
criptoGame _  4 =  print("Fim")
criptoGame generatedList value = do 
    userList <- loopInput
    let resultCompleto = compareCode userList generatedList
    let newUserList = clearList userList resultCompleto
    let newGeneratedList = clearList generatedList resultCompleto
    let parcialBool = listOfFalse (newGeneratedList)
    let completo = countResult resultCompleto
    let resultParcial= comparePartialCode (newUserList) (newGeneratedList) (parcialBool)
    let parcial = countResult resultParcial
    print(show completo ++ " Completo, " ++ show (parcial) ++ " Parcial")
    criptoGame generatedList completo

checkInput :: [Int] -> Bool
checkInput [] = True
checkInput listInput = do
    if (head listInput) < 7 && (head listInput) > 0 then
        checkInput (tail listInput) 
    else 
        False

inputlength :: [Int] -> Integer
inputlength [] = 0
inputlength (_:xs) = 1 + inputlength xs


loopInput :: IO[Int]
loopInput = do
    putStrLn "?"
    line <- getLine 
    let userList = map (read::String->Int) (splitOn  " " line)
    let check = (checkInput userList)
    let lenght = (inputlength userList)
    if check == True && lenght == 4 then
        return userList
    else do
        putStrLn " "
        putStrLn "Entrada invalida"
        loopInput


main :: IO ()
main = do
    generatedList <- randomList 4
    criptoGame generatedList 0


    