--Cordun Diana-Alexandra
--Grupa 242
import Data.List

type Name = String --val string asociate numelor de variabile

data Value = VBool Bool --val asoc expresiilor bool,int, fct, err
  |VInt Int
  |VFun (Value -> Value)
  |VError

data Hask = HTrue | HFalse
  |HIf Hask Hask Hask
  |HLit Int
  |Hask :==: Hask
  |Hask :+: Hask
  |Hask :*: Hask --operatia de inmultire pentru expresii
  |HVar Name
  |HLam Name Hask
  |Hask :$: Hask

infix 4 :==:
infixl 6 :+:
--4.2
infixl 7 :*:
infixl 9 :$:

type HEnv = [(Name, Value)] --mediu de evaluare al expresiilor

--1.
instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VFun f) = show "Functiile nu se pot afisa"
  show (VError) = show "Erorile nu se pot afisa"

--2.
instance Eq Value where
    VBool a == VBool b = a == b
    VInt a == VInt b  = a == b
    VFun a == VFun b   = error "Functiile nu se pot compara"
    VError == VError = error "Erorile nu se pot compara"

--3.
mylookUp :: HEnv -> Name -> Value
mylookUp var name = head [ v | (n,v) <- var, name == n ]
--lookup din data.list pt a cauta o variabbila
hEval :: Hask -> HEnv -> Value
hEval HTrue r = VBool True
hEval HFalse r = VBool False
hEval (HIf c d e) r = hif (hEval c r) (hEval d r) (hEval e r)
  where hif (VBool b) v w = if b then v else w
        hif _ _ _ = VError
hEval (HLit i) r = VInt i --i de la int r= [(Name, Value)]
hEval (c :==: d) r = egalitate (hEval c r)(hEval c r)
  where egalitate (VInt i)(VInt j) = VBool (i == j)
hEval (c :+: d) r = suma (hEval c r)(hEval d r)
  where suma (VInt i)(VInt j) = VInt (i + j)
hEval (c :*: d) r = produs (hEval c r)(hEval d r) --4.2
  where produs (VInt i)(VInt j) = VInt (i * j)
--hEval (HVar name) r == lookup r name
hEval (HLam nume val) r = VFun (\x -> hEval val ((nume,x):r))
--hEval (c :$: d) r = happ (hEval c r) (hEval d r)
--  where happ (VFun f) v = f v

--4.
run :: Hask -> String
run pg = show (hEval pg [])

--4.1
h0 =(((HLam "x" (HLam "y" ((HVar "x") :+: (HVar "y")))) :$: (HLit 3)) :$: (HLit 4))
test_h0 = (hEval h0 []) == (VInt 7)
--4.3 cu error, in loc de VError sa preciez motivul
--INTREBARI
--3nu m am prins ce trebuie facut la :$:
--3mi am scris propia lookup, dar nu merge
