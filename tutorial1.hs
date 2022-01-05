import Data.List
--Exercise #1
--nu puteam numi functia elem, asa ca am numit-o f
e :: (Eq a) => a -> [a] -> Bool
e _ [] = False
e a (x:xs)
  | a == x = True
  | null xs  = False
  | otherwise = e a xs

--Rez Video
-- elem _ [] = False
-- elem e (x:xs) = (e == x) || (elem e xs)

-- --Exercise #2
-- n :: (Eq a) => [a] -> [a]
-- n [] = []
-- n (x:xs)
--   | e x xs = n xs
--   | otherwise = x : n xs

--Rez video
-- nub [] = []
-- nub (x:xs)
--   | x `elem` xs = nub xs
--   | otherwise = x : nub xs

--Exercise #3

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc (x:xs)
  |  null xs = True
  |  x <= head xs = isAsc xs
  | otherwise = False

--Rez Video
-- isAsc [] = True
-- isAsc [x] = True
-- isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)


--Exercise #4
hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
  | x  == y = True
  | otherwise =
    let xs' = [ (n,m) | (n,m) <- xs, n /= x] in
    or [ hasPath xs' m y | (n,m) <- xs, n==x ]


--Rez Video
