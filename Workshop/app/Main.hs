{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use foldr" #-}
{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main
  ( main,
  )
where

import Data.Foldable (Foldable (foldr'))
import Data.Text (Text)
import qualified Data.Text as T

-- Für Verarbeitung von Daten auf Byteebene: bytestring

{-
 Block kommentar
-}

-- Zeilenkommentar

main :: IO ()
main =
  putStrLn "Hello, Haskell"

string = "Hallo"

zahlen :: Int
zahlen = 5

unit = ()

-- Bottom
endlos :: t
endlos = endlos

nochNichtFertig :: String
nochNichtFertig = undefined

---

-- Funktionen

plus10 :: Int -> Int
plus10 x = x + 10

zwangzig :: Int
zwangzig = plus10 10

-- Achtung:

-- >>> plus10 10 * 5
-- 100

-- >>> (plus10 10) * 5
-- 100

-- >>> plus10 (10 * 5)
-- 60

-- Funktion: doppelt?

doppelt :: Int -> Int
doppelt x = x * 2

---

-- Einschub

{-
   plus10 5
= { Definition plus10 }
   5 + 10
= { nach Def. + }
   15
-}

immerEins :: Int -> Int
immerEins _ = let x = 5 in x - 4

---

-- let ... in ...

mystery :: Int -> Int
mystery x =
  let p10 y = y + 10
   in p10 x + p10 x

-- where
mystery' :: Int -> Int
mystery' x =
  p10 x + p10 x
  where
    p10 y = y + 10

-- Lambda \x -> x+1
plus1 :: Int -> Int
plus1 = \x -> x + 1

-- gibts schon - id
genFun :: a -> a
genFun x = x

---

-- Funktionen mit mehreren Parametern

-- Currying
plus :: Int -> (Int -> Int)
plus x y = x + y

plus' :: (Int, Int) -> Int
plus' (x, y) = x + y

plus10_T :: Int -> Int
plus10_T x = plus' (10, x)

plus10_C :: Int -> Int
plus10_C = plus 10

-- auf Typ-Ebene
-- f :: a -> b -> c = a -> (b -> c)
-- auf Wert-Ebene (Anwenden der Funktion)
-- f x y = (f x) y

-- Quiz
-- f :: (a -> b) -> a -> b
-- g :: (a -> a) -> a -> a

quiz1 :: (a -> b) -> a -> b
quiz1 f_a_b a = f_a_b a

quiz2 :: (a -> a) -> a -> a
quiz2 f_a_a a = f_a_a (f_a_a a)

---

-- Funktionen-Verkettung
-- Operator (.)

plus11 :: Int -> Int
plus11 = plus10 . plus1

-- andere Sprachen

-- | > (| >)
--  pluss11 x = x |> plus1 |> plus10
--  (&) in Data.Function

---

-- Fakultät - 5! = 1 * 2 * 3 * 4 * 5 // n! 1 * 2 * ... * n // 0! = 1
fact :: Int -> Int
fact x =
  if x <= 0 then 1 else x * fact (x - 1)

-- mit "Guards"
fact' :: Int -> Int
fact' x
  | x <= 0 = 1
  | otherwise = x * fact (x - 1)

-- Fibonacci-Funktion
-- fib n falls n <= 0 = 0
-- fib n falls n == 1 = 1
-- fib n sonst = fib (n-1) + (n+2)

fib :: Int -> Int
fib n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n + 1)

---

-- data - Datentypen selber definieren

data Boolean
  = Wahr
  | Falsch
  deriving (Show)

nicht :: Boolean -> Boolean
nicht Wahr = Falsch
nicht Falsch = Wahr

und :: Boolean -> Boolean -> Boolean
und Wahr x = x
und Falsch _ = Falsch

oder :: Boolean -> Boolean -> Boolean
oder Falsch x = x
oder Wahr _ = Wahr

data Benutzer
  = Unbekannt
  | Name String
  deriving (Show)

begruessung :: Benutzer -> String
begruessung Unbekannt = "Hallo Fremder!"
begruessung (Name "Paul") = "Prost"
begruessung (Name name)
  | name == "Carsten" = "Hallo Trainer"
begruessung (Name name) = "Hallo " ++ name ++ "!"

begruessung' :: Benutzer -> String
begruessung' benutzer =
  case benutzer of
    Unbekannt -> "Hallo Fremder!"
    (Name name) -> "Hallo " ++ name ++ "!"

data Option a
  = None
  | Some a
  deriving (Show)

withDefault :: ta -> Option ta -> ta
withDefault def None = def
withDefault _ (Some valA) = valA

myMap :: (a -> b) -> Option a -> Option b
myMap _ None = None
myMap f (Some a) = Some (f a) -- None

-- Functor Laws
-- myMap id opt == opt
-- myMap (f . g) == (myMap f) . (myMap f)

---

-- Listen

data Liste a
  = Nil
  | Cons a (Liste a)
  deriving (Show)

einsZweiDrei :: Liste Int
einsZweiDrei =
  Cons 1 (Cons 2 (Cons 3 Nil))

{-
data [] a
   = []
   | a : [a]
-}

einsZweiDrei' :: [Int]
einsZweiDrei' =
  1 : (2 : (3 : []))

einsZweiDrei'' :: [Int]
einsZweiDrei'' =
  [1, 2, 3]

-- oder [1..3]

-- head/tail

head' :: [a] -> a
head' (h : _) = h
head' [] = error "empty list"

safeHead' :: [a] -> Maybe a
safeHead' (h : _) = Just h
safeHead' [] = Nothing

tail' :: [a] -> [a]
tail' (_ : tl) = tl
tail' [] = error "empty list"

summe :: [Int] -> Int
summe [] = 0
summe (z : zs) = z + summe zs

{-

summe [1,2,3] =
1 + summe [2,3] =
1 + (2 + summe [3]) =
1 + (2 + (3 + summe [])) =
1 + (2 + (3 + 0))

-}

summe' :: [Int] -> Int
summe' = go 0
  where
    go !acc [] = acc
    go !acc (z : zs) = go (acc + z) zs

{-

Idee FoldR:

  foldr f s ( a : b : c : []) =
      a `f` (b `f` (c `f` s))

-}

summe'' :: [Int] -> Int
summe'' = foldr (+) 0

---

{-

Eingabe: Betrag (3.23€)
Ausgabe: [200, 100, 20, 2, 1]

-}

type Muenze = Int

type Betrag = Int

muenzen :: [Muenze]
muenzen = [200, 100, 50, 20, 10, 5, 2, 1]

{-

Idee:

zuWechselnderBetrag / Rest-Muenzvorrat
ist erste Münze in Rest kleiner/größer zuWechselnder Betrag

wechsle 323 [200,100,50,20,5,2,1] = 200 : [...(rekursiv) ]
wechsle 123 [200,100,50,20,5,1,1] = [...(rekursiv)]

-}

wechsle :: Betrag -> [Muenze] -> [Muenze]
wechsle restBetrag alles@(muenze : rest)
  | restBetrag >= muenze = muenze : wechsle (restBetrag - muenze) alles
  | otherwise = wechsle restBetrag rest
wechsle _ [] = []

---

-- List Comprehensions

quadratZahlen :: [Integer]
quadratZahlen = [z * z | z <- [1 ..]] -- map (\z -> z*z) [1..]

geradeZahlen :: [Integer]
geradeZahlen = [z | z <- [1 ..], even z] -- filter even [1..]

strange :: [Integer]
strange = [g | z <- [1 ..], even z, let g = z + 1] -- filter even [1..]

fruechte :: [String]
fruechte = ["Apfel", "Birne", "Orange"]

kombinationen :: [(Int, String)]
kombinationen = [(n, frucht) | n <- [1, 2, 3], frucht <- fruechte]

durchNummeriert :: [(Int, String)]
durchNummeriert = zip [1 ..] fruechte

durchNummeriert' :: [(Int, String)]
durchNummeriert' = [(n, fruechte !! n) | n <- [0 .. length fruechte - 1]]

zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
zip' _ _ = []

-- Sudoko Solver mit List Comprehnsions https://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf

----

{-

Für Listen

map (f . g) xs = (map f . map g) xs

map f [] = []
map f (x:xs) = f x : map f xs

Fall 1 xs = []
--------------

   map (f . g) []
= { Def map - 1 Zeile }
= []

   (map f . map g) []
= map f (map g [])
= map f []
= []

Ind. Fall 2 xs = x:xs'
---------------------
Darf verwenden map (f.g) xs' = (map f . map g) xs'

   map (f . g) (x:xs)
= (f . g) x : map (f . g) xs'
= (f . g) x : (map f . map g) xs'
= f (g x) : map f (map g xs')
= map f (g x : map g xs')
= map f (map g (x:xs'))
= (map f . map g) (x:xs')

-}

---

-- Typ Klassen

class MyShow a where
  myShow :: a -> String

instance MyShow Boolean where
  myShow Wahr = "Wahr"
  myShow Falsch = "Falsch"

class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

instance MyFunctor Liste where
  myFmap _ Nil = Nil
  myFmap f (Cons x xs) = Cons (f x) (myFmap f xs)

applyShow :: (MyShow a, MyFunctor f) => f a -> f String
applyShow = myFmap myShow

---

hallo :: IO ()
hallo = do
  putStrLn "Name?"
  name <- getLine
  putStrLn ("Hallo " ++ name)

hallo' :: IO ()
hallo' =
  putStrLn "Name?" >>= (\() -> getLine >>= (\name -> putStrLn ("Hallo " ++ name)))

---
