

--1
poly2 :: Double -> Double -> Double -> Double -> Double

poly2 a b c x = let

                u=x*x

                v= b*x

                in a*u+v+c

--2
eeny :: Integer -> String
eeny n = if even n then "eeny" else "meeny"

--3
--varianta cu cazuri
fizzbuzz1 :: Integer -> String
fizzbuzz1 x = if mod x 15 == 0
  then "fizzbuzz"
  else if mod x 3 == 0
    then "fizz"
    else if mod x 5 == 0
      then "buzz"
      else ""

--varianta cu garzi
fizzbuzz :: Integer -> String
fizzbuzz x
  | mod x 15 == 0  = "fizzbuzz"
  | mod x 3 == 0 ="fizz"
  | mod x 5 == 0 ="buzz"
  | otherwise = ""

--4
tribonacci1 :: Integer -> Integer
tribonacci1 n
  | n < 3 = 0
  | n == 3 = 1
  | otherwise = tribonacci1(n-1) + tribonacci1(n-2) + tribonacci1(n-3)

tribonacci2 :: Integer -> Integer
tribonacci2 1 = 0
tribonacci2 2 = 0
tribonacci2 3 = 1
tribonacci2 n = tribonacci2(n-1) + tribonacci2(n-2) + tribonacci2(n-3)

--5
binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

--6 a)
verifL :: [Int] -> Bool
verifL = even . length

--6 b)
takefinal :: [Int] -> Int -> [Int]
takefinal [] _ = []
takefinal (x:xs) n --x:xs inseamna ca are minim un element
  | length (x:xs) <= n = (x:xs) --len listei este mai mica decat n
  | otherwise = takefinal xs n --renunt la x

--6 b// sa mearga si pe String
  takefinal :: [a] -> Int -> [a]
  takefinal [] _ = []
  takefinal (x:xs) n
  | length (x:xs) <= n = (x:xs)
  | otherwise = takefinal xs n

--6 c)
remove :: [a] -> Integer -> [a]
remove xs n = take (n-1) xs ++ drop n xs --ia primele n-1 elemente din xs si concateneaza la lista fara urmatoarele n elemente

--7 a)
myreplicate :: Integer -> Double -> [Double]
myreplicate 0 v = []
myreplicate n v
  | n > 0 = v : myreplicate (n-1) v --cat timp n>0 tot intorc v-uri

--7 b)
sumImp :: [Integer] -> Integer
sumImp [] = 0
sumImp (z:zs)
   | mod z 2 == 0 = sumImp zs
   | otherwise = z + sumImp zs

--7 c)
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (z:zs)
  | head x == 'A' = length x + totalLen zs
  | otherwise = totalLen zs
