--Cordun Diana-Alexandra
--Grupa 242

--Exercitiul 1
--a)
sfChr :: Char -> Bool
sfChr c = if c `elem` ".?!:" then True else False

nrPropozitii :: String -> Int
nrPropozitii "" = 0
nrPropozitii (t:text)
  | sfChr t = 1 + nrPropozitii text
  | otherwise = nrPropozitii text

--b)
nrPropozitii2 :: String -> Int
nrPropozitii2 text = length [ x | x <- text, elem x ".?!:" ]

--Exercitiul 2
liniiN :: [[Int]] -> Int -> Bool
liniiN mat n = foldr (&&) True [ n == (length.filter (\x -> x > 0)) l | l <- mat, length l == n]

--Exercitiul 3
data Punct = Pt [Int] --punct cu nr variabil de coordonate intregi
            deriving Show

data Arb = Vid | F Int | N Arb Arb  --arbori cu informatia in frunze (F)
            deriving Show

class ToFromArb a where --clasa de tipuri
      toArb :: a -> Arb
      fromArb :: Arb -> a

instance ToFromArb Punct where --lista [int] == frontiera arborelui
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
  fromArb = Pt . fromArb' where
      fromArb' Vid = []
      fromArb' (F x) = [x]
      fromArb' (N s d) = fromArb' s ++ fromArb' d
