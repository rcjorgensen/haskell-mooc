
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.List
import Data.IORef
import System.IO

-- here comes the tree monad
data Tree a = Leaf a | Node a (Tree a) (Tree a)
data MultiTree a = MultiTree a [MultiTree a] 
    deriving Show

--instance Monad MultiTree where

-- return 
eta :: a -> (MultiTree a)
eta a = MultiTree a []


getnode :: (MultiTree a) -> a
getnode (MultiTree n trees) = n

getbranches :: (MultiTree a) -> [MultiTree a]
getbranches (MultiTree n trees) = trees

--addnodes :: (MultiTree a) -> (MultiTree a) -> (MultiTree a)
--addnodes t nodes | t 

--idea: input (t, trees) add trees to end of every leaf of t
--reduceTree :: (MultiTree (MultiTree a)) -> MultiTree a
--reduceTree MultiTree a trees = MultiTree (getnode a) []

-- chain
mu ::  (a -> MultiTree b) -> MultiTree a  -> MultiTree b
mu f (MultiTree t1 ts) = MultiTree (getnode (f t1)) ((getbranches (f t1)) ++ (map (mu f) ts))

 --kopieret herfra: https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/
instance Functor MultiTree where
   fmap f (MultiTree x treeList) = MultiTree (f x) (map (fmap f) treeList)
 
 --kopieret herfra: https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/
instance Applicative MultiTree where
   pure x = MultiTree x []
   (MultiTree f treeFunctionList) <*> (MultiTree x treeElementList) =
      MultiTree (f x) ( (map (fmap f) treeElementList) ++ (map (<*> (MultiTree x treeElementList)) treeFunctionList) )
 
instance Monad MultiTree  where
    t >>= f      = mu f t
    return x            = eta x

--multitreesum :: MultiTree a -> a
--multitreesum (MultiTree t br) = t + Data.List.sum (map multitreesum br)

{- class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (Just _) >>  k      = k
    Nothing  >>  _      = Nothing

    return x            = Just x -}

printForUs :: IO ()
printForUs = putStrLn "hello"

--t = MultiTree 'a' [MultiTree 'b' [], MultiTree 'c' []]
t = MultiTree 1 [MultiTree 2 [], MultiTree 3 []]

tfunc :: a -> (MultiTree a)
tfunc x = MultiTree x [MultiTree x [], MultiTree x []]

main :: IO ()
main  = do  
            print("repetition fra lister")
            print("fmap",fmap (2*) [1,2,3])
            print("[(2*),(100+),(1000+)] <*> [1,2,3]")
            print("applicative functor", [(2*),(100+),(1000+)] <*> [1,2,3])
            print("monad", [1,2,3] >>= (\x -> [2*x,100+x,1000+x]))

            print("opførsel af monader")
            print ("fmap", fmap (*2) t)
            print ("applicative functor", (MultiTree (*2) [MultiTree (+100) [], MultiTree (+1000) []]) <*> t)
            print ("monad", t  >>= (\x -> MultiTree (x*2) [MultiTree (x+100) [], MultiTree (x+1000) []]))





--note: Hvis man laver en fejl i monadedefinitoen, saa faar man ingen advarsel - haskell koerer bare kaldene med maerkelige resultater.