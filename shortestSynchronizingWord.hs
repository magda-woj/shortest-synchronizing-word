import Prelude
import System.IO
import System.Environment

data DFA state = DFA {  states :: [state],
                        alphabet :: [Char],
                        transition:: state -> Char -> state
                    }

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort (filter (x >) xs) ++ [x] ++ quickSort (filter (x <=) xs)

uniqueSorted :: (Eq a) => [a] -> [a]
uniqueSorted xs = foldr (\e xs -> if e /= head xs then e:xs else xs) [last xs] xs

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let shorterSublists = sublists xs in
    [x:sublist | sublist <- shorterSublists] ++ shorterSublists

nonemptySublists :: [a] -> [[a]]
nonemptySublists xs = init (sublists xs)

powerAutomaton :: (Ord state) => DFA state -> DFA [state]
powerAutomaton (DFA sts al trans) = let powerStates = nonemptySublists sts in
    DFA powerStates al (\xs a -> uniqueSorted (quickSort (map (`trans` a) xs)))

neighbours :: Eq state => DFA state -> (state, String) -> [(state, String)]
neighbours (DFA sts al trans) (st, word) =  map (\ c -> (trans st c, word ++ [c])) al

bfs :: Eq state => DFA state -> [state] -> [(state, String)] -> [(state, String)]
bfs _ _ [] = []
bfs automaton visited (h:queue)
    | fst h `notElem` visited = h : bfs automaton (fst h : visited) (queue ++ filter (\(st, word) -> st `notElem` visited) (neighbours automaton h))
    | otherwise = bfs automaton visited queue


pathWords :: Ord state => DFA state -> [([state], String)]
pathWords automaton = let powAuto = powerAutomaton automaton in
    bfs powAuto [] [(head (states powAuto), "")]

synchronizingWords :: Ord state => DFA state -> [String]
synchronizingWords automaton = map snd (filter (\x -> length (fst x) == 1) (pathWords automaton))

shorter :: String -> String -> String
shorter s1 s2 = if length s1 <= length s2 then s1 else s2

shortestSynchronizingWord :: Ord state => DFA state -> String
shortestSynchronizingWord automaton = let synchro = synchronizingWords automaton in
    foldr shorter (head synchro) synchro


shortestWord :: [String] -> String
shortestWord xs = foldr shorter (head xs) xs

find :: (a -> Bool) -> [a] -> a
find condition xs = head (foldl (\list x -> if condition x then x:list else list) [] xs)

constructTransition :: [[(Char, Int)]] ->  (Int -> Char -> Int)
constructTransition xs n c = snd (find (\pair -> fst pair == c) (xs !! n))

rList :: String -> [[(Char, Int)]]
rList = read

main :: IO ()
main = do
        handle <- openFile "automaton.txt" ReadMode
        n <- hGetLine handle
        let numOfStates = read n :: Int
        alphabet <- hGetLine handle
        trans <- hGetLine handle
        hClose handle
        let synchro = synchronizingWords (DFA [0..numOfStates-1] alphabet (constructTransition (rList trans)))
        if null synchro
            then print "automaton isn't synchronizing"
            else print (shortestWord synchro)