--Cordun Diana-Alexandra
--Grupa 242
--1
firstEl :: [(a,b)] -> [a]
firstEl = map fst
--ia doar primul element din fiecare el al listei
--de ce nu ia argumente?

--2
sumList :: [[Int]] -> [Int]
sumList = map sum

--3
prel2 :: [Int] -> [Int]
prel2 = map (\x ->  if even x then x `div` 2 else x*2 )

--4
contine :: Char -> [String] -> [String]
contine c  = filter (c `elem`)

--5
patrate :: [Int] -> [Int]
patrate xs = map (\x -> x^2) $ filter (\x -> odd x) xs

--6
patratePoz :: [Int] -> [Int]
patratePoz xs = map ((^2).fst) $ filter (odd.snd) $ zip [1..] xs

--7
numaiVocale :: [String] -> [String]
numaiVocale  = map $ filter (`elem` "AEIOUaeiou")

--8
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) =  f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter pr (x:xs)
  | pr x = x : myfilter pr xs
  | otherwise = myfilter pr xs

--9
sumaPatrate :: [Int] -> Int
sumaPatrate = sum.map (^2).filter odd

--10
vf :: [Bool] -> Bool
vf = foldr (&&) True

--11a
rmChar :: Char -> String -> String
rmChar c = foldr f []
  where f x | c == x = id
            | otherwise = (x:)

--sau rezolvarea Denisei
--rmChar :: Char -> String ->String
--rmChar c = filter (c /= )

--11b
rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec [] y = y
rmCharsRec (x:xs) y
        | x `elem` y = rmCharsRec xs (rmChar x y)
        | otherwise = rmCharsRec xs y

--11c
rmCharsFold :: String -> String -> String
rmCharsFold x = foldr f []
          where f y |  y `elem` x = id
                    | otherwise = (y:)

--varianta profului,foloseste rmChar
rmCharsFold2 :: String -> String -> String
rmCharsFold2 s1 s2 = foldr (rmChar) s2 s1
