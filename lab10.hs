--Cordun Diana-Alexandra
--Grupa 242
import Data.Maybe
--1.Expresii si Arbori
data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
            deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
            deriving (Eq, Show)

--1.1.
instance Show Expr where
  show (Const x) = show x
  show (x :*: y) = "(" ++ show x ++ "*" ++ show y ++ ")"
  show (x :+: y) = "(" ++ show x ++ "+" ++ show y ++ ")"

--1.2.
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :*: y) = (evalExp x) * (evalExp y)
evalExp (x :+: y) = (evalExp x) + (evalExp y)

--toate testele sunt True
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16


--1.3.
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Mult tree1 tree2) = (evalArb tree1) * (evalArb tree2)
evalArb (Node Add tree1 tree2) = (evalArb tree1) + (evalArb tree2)

--toate testele sunt True
arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)
test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

--1.4. --tsfm o expresie in arbore
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (x :*: y) = Node Mult (expToArb x) (expToArb y)
expToArb (x :+: y) = Node Add (expToArb x) (expToArb y)

--2. Clasa Collection
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key
      => key -> value -> c key value -> c key value
  mylookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys c = [fst p | p <- toList c] --2.1. a.
  values :: c key value -> [value]
  values c = [snd p | p <- toList c] --2.1. b.
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value --2.1. c.
  fromList ((k,v):es) = insert k v (fromList es)


--2.2.
newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty = PairList [] --lista goala
  singleton k v = PairList[(k,v)] --un singur obiect de acest tip
  insert k v (PairList x) = PairList $ (k,v) : [(a,b) | (a,b) <- x, a /= k]
  mylookup k (PairList []) = Nothing
  mylookup k (PairList (x:xs))
    | fst x == k = Just (snd x)
    | otherwise = mylookup k (PairList xs)
  delete k (PairList x) = PairList [(a,b) | (a,b) <- x, a /= k]
  keys (PairList x) = [a | (a,b) <- x]
  values (PairList x) = [b | (a,b) <- x]
  toList (PairList x) = [(a,b) | (a,b) <- x ]
  fromList ((k,v):es) = insert k v (fromList es)

--2.3.
data SearchTree key value
  = Empty
  | BNode
    (SearchTree key value) -- elemente cu cheia mai mica
    key -- cheia elementului
    (Maybe value) -- valoarea elementului
    (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  insert k v = go
     where
       go Empty = singleton k v
       go (BNode t1 k1 v1 t2)
         | k == k1   = BNode t1 k1 (Just v) t2
         | k < k1    = BNode (go t1) k1 v1 t2
         | otherwise = BNode t1 k1 v1 (go t2)
  mylookup k Empty = Nothing
  mylookup k (BNode t1 k1 v1 t2)
    | k1 == k = v1
    | k < k1  = mylookup k t1 --caut unde e cheia mai mica
    | otherwise = mylookup k t2
  delete _ (Empty) = Empty
  delete k (BNode t1 k1 v1 t2)
    | k1 == k = (BNode t1 k1 Nothing t2)
    | k < k1  = (BNode (delete k t1) k1 v1 t2)
    | otherwise = (BNode t1 k1 v1 (delete k t2))
  keys (BNode t1 k1 (Just v1) t2) = keys t1 ++ [k1] ++ keys t2
  keys (BNode t1 k1 Nothing t2) = keys t1 ++ keys t2
  keys _ = []
  values (BNode t1 k1 (Just v1) t2) = values t1 ++ [v1] ++ values t2
  values (BNode t1 k1 Nothing t2) = values t1 ++ values t2
  values _ = []
  toList (BNode t1 k1 (Just v1) t2) = toList t1 ++ [(k1, v1)] ++ toList t2
  toList (BNode t1 k1 Nothing t2) = toList t1  ++ toList t2
  toList _ = []
  fromList ((k,v):es) = insert k v (fromList es)
