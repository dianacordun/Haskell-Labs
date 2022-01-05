--Cordun Diana-Alexandra
--Grupa 242
{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}

newtype Identity a = Identity a
data Pair a = Pair a a
data Constant a b = Constant b
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four'' a b = Four'' a a a b
data Quant a b = Finance | Desk a | Bloor b

--fmap :: (a -> b) -> Identity a -> Identity b
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b) --pair este constructor de date si primeste 2 argumente

instance Functor (Constant a) where --constructor de tip
  fmap f (Constant b) = Constant (f b)--constructor de date

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
  --se pune doar f b, functorul ia un kind * si il duce in *
  --la pair a mers pt ca este vorba de acelasi tip de parametru
  --provide functor instance by reducing its type kindness
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
  fmap f (Four a b c d ) = Four a b c (f d)

instance Functor (Four'' a) where
  fmap f (Four'' a b c d ) = Four'' a b c (f d)  --a,b,c sunt de tip a, d este de tip b din constr de tipdata LiftItOut f a = LiftItOut (f a)


instance Functor (Quant a) where
  fmap f (Finance) = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)


data LiftItOut f a = LiftItOut (f a) --constructorul ia o functie, f este functor
instance Functor f => Functor (LiftItOut f) where --spunem ca f e functor
  fmap g (LiftItOut f) = LiftItOut (fmap g f)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g =>  Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g =>  Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap h (NoGoat) = NoGoat
  fmap h (OneGoat a1) = OneGoat (h a1)
  fmap h (MoreGoats a b c ) = MoreGoats (fmap h a) (fmap h b) (fmap h c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap h (Halt) = Halt
  fmap h (Print s a) = Print s (h a)
  fmap h (Read f) = Read (h.f)
