module DList where

import Data.List

type Len = Int
type Pos = Int
data Circ a = Circ Pos Len [a]
  deriving Show

emp :: Circ a
emp = Circ 0 0 []

ins :: a -> Pos -> Circ a -> Circ a
ins x px (Circ p l xs)
	| 
	where
	  insRaw xs = x : (front ++ (insRaw back))
	  (front,back) = splitAt l xs -- Is `xs` bound by `insRaw` or `ins` here?
	  pNorm = let pMod = p % l in
	    	  if pMod < px then pMod else pMod - l

first (

--instance Circ of Monad where
    

