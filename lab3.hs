--Cordun Diana, 242
import Data.Char

--1
nrVocale :: [String] -> Int
nrVocale [] = 0 --cazul de sir vid
nrVocale (h:r)
    | h == reverse h = --daca e palindrom
      let
          vocale :: String -> Int --functie auxiliara care imi calculeaza nr de vocale
          vocale "" = 0 --daca este cuvantul vid
          vocale (z:zs)
            | z `elem` "aeiou" = 1 + vocale zs --functia elem verifica daca lista contine
            | otherwise = vocale zs
      in
       vocale h + nrVocale r --calculez vocale de head si fac recursiv cu restul listei
    | otherwise = nrVocale r --daca nu e palindrom, trec la urmatorul cuvant

--2
f :: Integer -> [Integer] -> [Integer]
f _ [] = []
f x (z:zs)
  |z `mod` 2 == 0 = z:x:(f x zs)
  |otherwise = f x zs

--3
divizori :: Integer -> [Integer] 
divizori x = [y | y <- [1..(x `div` 2)], x `mod` y == 0  ] ++ [x]

--4
listadiv :: [Integer] -> [[Integer]]
listadiv ls = [divizori x | x <- ls]

--5a //recursie
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec x y (l:ls)
  | x <= l && l <= y = [l] ++ inIntervalRec x y ls
  | otherwise = inIntervalRec x y ls

--5b //descrieri de liste
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y ls = [z | z <- ls, x <= z && z <= y]


--6a //doar recursie
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (l:ls)
  | l > 0 = 1 + pozitiveRec ls
  | otherwise = pozitiveRec ls

--6b //descrieri de liste
--nu putem folosi doar descrieri de liste deoarece functia intoarce un Int, nu o lista de Int-uri
pozitiveComp :: [Int] -> Int
pozitiveComp ls = length [x | x <- ls, x > 0]

--7a //doar recursie
pozitiiImpareRecAux :: Int -> [Int] -> [Int]
pozitiiImpareRecAux _ [] = []
pozitiiImpareRecAux i (l:ls)
  | l `mod` 2 == 1 = i : pozitiiImpareRecAux (i+1) ls
  | otherwise = pozitiiImpareRecAux (i+1) ls

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec ls = pozitiiImpareRecAux 0 ls

--7b //descrieri de liste
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp ls = [x | (i,x) <- ls `zip` [0..], odd i]


--8a //doar recursie

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (s:ss)
  |isDigit s = digitToInt s * multDigitsRec ss
  |otherwise = multDigitsRec ss

--8b //descrieri de liste
multDigitsComp :: String -> Int
multDigitsComp s = product [digitToInt x | x <- s, isDigit x]
