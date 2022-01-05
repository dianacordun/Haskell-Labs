--Cordun Diana-Alexandra
--Grupa 242

--1
factori :: Int -> [Int]
factori x = [ d |  d <- [1..x `div `2], rem x d == 0 ] ++ [x]

--2
prim :: Int -> Bool
prim x = if factori x == [1,x] || x == 1 then True else False

--3
numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n] , prim x ]

--4
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int,Int,Int)]
myzip3 [] [] [] = []
myzip3 _ _ [] = []
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs

--5

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [y <= z | (y, z) <- zip (x:xs) xs]

--6
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs)
  | x > head xs = False
  | otherwise =  ordonataNat1 xs

--7a
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [x] _ = True
ordonata (x:xs) op = and[ x `op` head xs, ordonata xs op]

--7b
auxd :: Int -> Int  -> Bool
auxd a b = b `mod` a == 0


--8
infixr 6 *<*
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(*<*) (a,b) (c,d)  = a > c && b < d

--9
compuneList :: (b -> c) -> [(a -> b)] -> [( a -> c)]
compuneList f [] = []
compuneList f (x:xs) = f.x : compuneList f xs

--10
aplicaList :: a -> [(a -> b)] -> [b]
aplicaList n [] = []
aplicaList n (x:xs) = x n : aplicaList n xs
