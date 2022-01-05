myInt = 55555555555555555555555555555555555555555555555555555555555
double :: Integer -> Integer
double x = x+x
triple x = double x + x
patrat :: Integer -> String
patrat x = if even x then "par" else "impar"

{-
6. Să se scrie următoarele funct, ii:
a) functie cu 2 parametri care calculeaza suma pătratelor celor două numere;
b) funct, ie cu un parametru ce întoarce mesajul “par” dacă parametrul este par s, i “impar” altfel;
c) funct, ie care calculează factorialul unui număr;
d) funct, ie care verifică dacă un primul parametru este mai mare decât dublul celui de-al doilea
parametru. -}

--a

suma_patrate :: Integer -> Integer -> Integer
suma_patrate x y = let u = x*x ; z= y*y in u+z  --returneaza u+z

--b

paritate :: Integer -> String
paritate x = if even x then "par" else "impar"
--x 'mod' 2==1

--c

factorial :: Integer -> Integer
factorial x = product[1..x]

--d

dublu :: Integer -> Integer -> Bool
dublu x y = if x > y*2 then True else False
