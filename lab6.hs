--Cordun Diana-Alexandra
--Grupa 242
import Data.Maybe

--Exercitiul 1
data Fruct = Mar String Bool | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
  Portocala "Sanguinello" 10,
  Portocala "Valencia" 22,
  Mar "Golden Delicious" True,
  Portocala "Sanguinello" 15,
  Portocala "Moro" 12,
  Portocala "Tarocco" 3,
  Portocala "Moro" 12,
  Portocala "Valencia" 2,
  Mar "Golden Delicious" False,
  Mar "Golden" False,
  Mar "Golden" True]

--a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala tip felii)
    | tip == "Tarocco" || tip == "Moro" || tip == "Sanguinello" = True
    | otherwise = False

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12)
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True)

--b)
getFelii :: Fruct -> Int
getFelii (Portocala _ felii) = felii
getFelii (_) = 0

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs)
  | ePortocalaDeSicilia x = getFelii x + nrFeliiSicilia xs
  | otherwise = nrFeliiSicilia xs

test_nrFeliiSicilia = nrFeliiSicilia listaFructe

--c)
isMarCuViermi :: Fruct -> Bool
isMarCuViermi (Mar _ v) = v
isMarCuViermi (_) = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs)
  | isMarCuViermi x = 1 + nrMereViermi xs
  | otherwise = nrMereViermi xs

test_nrMereViermi = nrMereViermi listaFructe

--Exercitiul 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa deriving Show

--a)
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

--b)
rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ r) = Just r

test_rasa1 = rasa (Pisica "Weiss")
test_rasa2 = rasa (Caine "Pufi" "Labrador")

--Exercitiul 3
data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

--a)
verifica :: Matrice -> Int -> Bool
verifica (M mat) n = foldr (&&) True (map (\ (L l) -> sum l == n) mat)


test_veri1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25

--b)
elPoz :: Linie -> Bool
elPoz (L l) = ( length.filter (>0) $ l ) == length l

lungime :: Linie -> Int -> Bool
lungime (L l) n = n == length l

doarPozN :: Matrice -> Int -> Bool
doarPozN (M mat) n = foldr (&&) True [elPoz l | l <- mat, lungime l n]


testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3

c)
corect :: Matrice -> Bool
corect (M []) = True
corect (M [m]) = True
corect (M (m:mat))
    | length m == length (head xs) = corect (M xs)
    | otherwise False
