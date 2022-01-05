--Cordun Diana-Alexandra
--Grupa 242
import Data.Char
import Data.List

-- 1.
rotate::Int->[Char]->[Char]
rotate n (x:xs)
      | n == 0 = x:xs
      | n < 0 || n > length (x:xs) = error "n invalid"
      | n > 0  = rotate (n-1) (xs ++ [x])

-- 2.
--functia testeaza daca rotate functioneaza corespunzator
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                       where l = length str
                             m = if l == 0 then 0 else k `mod` l

--3.
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] l
              where l = rotate n ['A'..'Z']

--4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch (l:ls)
  | fst(l) == ch = snd(l)
  | ls == [] = ch
  | otherwise = lookUp ch ls

-- 5.
encipher :: Int -> Char -> Char
encipher key ch = lookUp ch $ makeKey key

-- 6.
normalize :: String -> String
normalize "" = ""
normalize (s:str)
  | isAlphaNum s = if isLower s then toUpper s : normalize str
                   else s : normalize str
  | otherwise =  normalize str


-- 7.
encipherStr :: Int -> String -> String
encipherStr key str = acc key (normalize str)
    where acc _ "" = ""
          acc key (s:str)
            | isAlpha s = encipher key s : acc key str
            | otherwise = s : acc key str
--sau
--encipherStr nr list = [encipher nr carac | carac <- normalize list]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey lkey = [(b,a) | (a,b) <- lkey ]

-- 9.
decipher :: Int -> Char -> Char
decipher key ch = lookUp ch $ reverseKey $ makeKey key

decipherStr :: Int -> String -> String
decipherStr _ "" = ""
decipherStr key (s:str) = decipher key s : decipherStr key str
