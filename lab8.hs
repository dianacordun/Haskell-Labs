--Cordun Diana-Alexandra
--Grupa 242
import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String
data Prop
  = Var Nume --numele propozitiei, p sau q
  | F --false
  | T --true
  | Not Prop --negatia
  | Prop :|: Prop --disjunctia, sau
  | Prop :&: Prop --conjunctia, si
  | Prop :->: Prop --9.implicatia
  | Prop :<->: Prop --9.echivalenta
  deriving Eq


infixr 1 :->:
infixr 1 :<->:
infixr 2 :|:
infixr 3 :&:

--Exercitiul 1
--1
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--2
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

--3
p3 :: Prop
p3 = (Var "P"  :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--Exercitiul 2
instance Show Prop where
  show (Not p) = "(~" ++ show p ++ ")"
  show (prop1 :|: prop2) = "(" ++ show prop1 ++ "|" ++ show prop2 ++ ")"
  show (prop1 :&: prop2) = "(" ++ show prop1 ++ "&" ++ show prop2 ++ ")"
  show (prop1 :->: prop2) = "(" ++ show prop1 ++ "->" ++ show prop2 ++ ")"
  show (prop1 :<->: prop2) = "(" ++ show prop1 ++ "<->" ++ show prop2 ++ ")"
  show (Var n) =  n
  show F = "F"
  show T = "T"

test_ShowProp :: Bool
test_ShowProp =
  show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

--Evaluarea expresiilor logice

type Env = [(Nume, Bool)]
--varianta lookup care genereaza eroare daca valoarea nu este gasita
--a gasi val asociata lui nume in env
impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

--Exercitiul 3
--Pentru Exercitiul 9, evaluarea expresiilor pentru implicatie si echivalenta
-- => exista deja in Haskell
(-->) :: Bool -> Bool -> Bool
True --> True = True
True --> False = False
False --> _ = True

(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
True <=> False = False
False <=> True = False

eval :: Prop -> Env -> Bool
eval (Var x) e = impureLookup x e
eval F _ = False
eval T _ = True
eval (Not x) e =  not (eval x e)
eval (prop1 :|: prop2) e = (eval prop1 e) || (eval prop2 e)
eval (prop1 :&: prop2) e = (eval prop1 e) && (eval prop2 e)
eval (prop1 :->: prop2) e = (eval prop1 e) --> (eval prop2 e)
eval (prop1 :<->: prop2) e = (eval prop1 e) <=> (eval prop2 e)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True


--Exercitiul 4
--nub scoate duplicatele
variabile :: Prop -> [Nume]
variabile (Var x) = [x]
variabile F = []
variabile T = []
variabile (Not x) = variabile x
variabile (prop1 :|: prop2) = nub (variabile prop1 ++ variabile prop2)
variabile (prop1 :&: prop2) = nub (variabile prop1 ++ variabile prop2)
variabile (prop1 :->: prop2) = nub (variabile prop1 ++ variabile prop2)
variabile (prop1 :<->: prop2) = nub (variabile prop1 ++ variabile prop2)

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--Exercitiul 5
--sequence face produsul cartezian
envs :: [Nume] -> [Env]
envs num = sequence [ [(x,y) |  y <- [False, True] ] | x <- num ]

test_envs = envs["P", "Q"] == [ [ ("P",False), ("Q",False)], [ ("P",False), ("Q",True)], [ ("P",True), ("Q",False)], [ ("P",True), ("Q",True)]]

--Exercitiul 6
satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p x | x <- (envs (variabile p))]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False


--Exercitiul 7
valida :: Prop -> Bool
valida p = not (satisfiabila (Not p))

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

--Exercitiul 10
--all intoarce True daca toate el din lista indeplinesc conditia
echivalenta :: Prop -> Prop -> Bool
echivalenta p q = all (\env -> eval (p :<->: q) env) $ envs $ nub $ variabile p ++ variabile q


test_echivalenta1 = True == (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 = False == (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 = True == (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))
