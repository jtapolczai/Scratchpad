{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module DependentTypes where

import Prelude hiding (head, tail, (++))
import qualified Prelude as P ((++))
import Data.List (intercalate)

data Nat = Z | S Nat

type family (:$) (s :: k -> p) (n :: k) :: p
type instance (:$) s n = s n

infixr 9 :$

type family (:+:) (n::Nat) (m::Nat) :: Nat
type instance (:+:) Z b = b
type instance (:+:) (S a) b = S (a :+: b)

type family (:*:) (n::Nat) (m::Nat) :: Nat
type instance (:*:) Z b = Z
type instance (:*:) (S a) b = b :+: (a :*: b)

type family (:^:) (n::Nat) (m::Nat) :: Nat
type instance (:^:) Z b = S Z
type instance (:^:) (S a) b = b :*: (a :^: b)

data List (n::Nat) a where
   Nil :: List Z a
   (:::) :: a -> List n a -> List (S n) a

infixr 5 :::

instance Functor (List k) where
   fmap _ Nil = Nil
   fmap f (h ::: t) = f h ::: fmap f t

instance Show a => Show (List k a) where
   show = (P.++ "]") . ("[" P.++) . intercalate "," . makeDynamic . fmap show

makeDynamic :: List k a -> [a]
makeDynamic Nil = []
makeDynamic (x ::: xs) = x : makeDynamic xs


head :: List (S n) a -> a
head (x ::: _) = x

tail :: List (S n) a -> List n a
tail (_ ::: xs) = xs

(++) :: List n a -> List m a -> List (n :+: m) a
Nil        ++ ys = ys
(x ::: xs) ++ ys = x ::: (xs ++ ys)

--(<*>) :: List k (a -> b) -> List p a -> List (k :*: p) b


class Applicative f where
   type Fpure
   type Fap k p
   type Fproxy f p a :: *

   pure :: a -> (Fproxy f) Fpure a
   (<*>) :: Fproxy f k (a -> b) -> Fproxy f p a -> Fproxy f (Fap k p) b

instance Applicative List where
   type Fpure = S Z
   type Fap k p = k :*: p

   type Fproxy List k a = List k a 

   pure = (:::Nil)

   --Nil <*> _ = Nil
   --(f:::fs) <*> xs = fmap f xs ++ (fs <*> xs)

l0 :: List Z Integer
l0 = Nil
l1 :: List (S :$ Z) Integer
l1 = 3 ::: l0
l2 :: List (S :$ S :$ Z) Integer
l2 = 2 ::: l1
l3 :: List (S :$ S :$ S :$ Z) Integer
l3 = 1 ::: l2

lf = (+1) ::: (*2) ::: Nil