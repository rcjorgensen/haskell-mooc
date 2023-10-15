
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

addnodes :: (MultiTree a) -> (MultiTree a) -> (MultiTree a)
addnodes t nodes | t 

--idea: input (t, trees) add trees to end of every leaf of t
reduceTree :: (MultiTree (MultiTree a)) -> MultiTree a
reduceTree MultiTree a trees = MultiTree (getnode a) []

-- chain
mu ::  (a -> MultiTree b) -> MultiTree a  -> MultiTree b
mu f (MultiTree t1 ts) = MultiTree getnode (f t1) (map (mu f) ts)


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

t = MultiTree 'a' [MultiTree 'b' [], MultiTree 'c' []]