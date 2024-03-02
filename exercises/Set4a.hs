-- Exercise set 4a:
--
--  * using type classes
--  * working with lists
--
-- Type classes you'll need
--  * Eq
--  * Ord
--  * Num
--  * Fractional
--
-- Useful functions:
--  * maximum
--  * minimum
--  * sort

module Set4a where

import Mooc.Todo
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Array

------------------------------------------------------------------------------
-- Ex 1: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--   allEqual [] ==> True
--   allEqual [1,2,3] ==> False
--   allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual xs = all (\x -> x == head xs) xs
-- allEqual xs = foldr (\x y -> x == head xs && y) True xs

------------------------------------------------------------------------------
-- Ex 2: implement the function distinct which returns True if all
-- values in a list are different.
--
-- Hint: a certain function from the lecture material can make this
-- really easy for you.
--
-- Examples:
--   distinct [] ==> True
--   distinct [1,1,2] ==> False
--   distinct [1,2] ==> True

distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

------------------------------------------------------------------------------
-- Ex 3: implement the function middle that returns the middle value
-- (not the smallest or the largest) out of its three arguments.
--
-- The function should work on all types in the Ord class. Give it a
-- suitable type signature.
--
-- Examples:
--   middle 'b' 'a' 'c'  ==> 'b'
--   middle 1 7 3        ==> 3
middle :: Ord a => a -> a -> a -> a
-- middle x y z = if x <= y then
--                             if y <= z then y
--                                       else if x <= z then z
--                                                      else x
--                         else
--                             if x <= z then x
--                                       else if y <= z then z
--                                                      else y
-- middle x y z = head (drop 1 (sort [x,y,z]))
middle x y z = b where [a,b,c] = sort [x,y,z]

------------------------------------------------------------------------------
-- Ex 4: return the range of an input list, that is, the difference
-- between the smallest and the largest element.
--
-- Your function should work on all suitable types, like Float and
-- Int. You'll need to add _class constraints_ to the type of range.
--
-- It's fine if your function doesn't work for empty inputs.
--
-- Examples:
--   rangeOf [4,2,1,3]          ==> 3
--   rangeOf [1.5,1.0,1.1,1.2]  ==> 0.5

rangeOf :: (Num a, Ord a) => [a] -> a
rangeof [] = 0
rangeOf xs = (-) (foldr (\x y-> max x y) (head xs) xs) (foldr (\x y-> min x y) (head xs) xs)
-- rangeOf xs = last s - head s where s = sort xs
------------------------------------------------------------------------------
-- Ex 5: given a (non-empty) list of (non-empty) lists, return the longest
-- list. If there are multiple lists of the same length, return the list that
-- has the smallest _first element_.
--
-- (If multiple lists have the same length and same first element,
-- you can return any one of them.)
--
-- Give the function "longest" a suitable type.
--
-- Challenge: Can you solve this exercise without sorting the list of lists?
--
-- Examples:
--   longest [[1,2,3],[4,5],[6]] ==> [1,2,3]
--   longest ["bcd","def","ab"] ==> "bcd"

-- longest :: Ord a => [[a]] -> [a]
-- longest l = longest' l []

-- longest' :: Ord a => [[a]] -> [a] -> [a]
-- longest' [] y = y
-- longest' (x:xs) y = if (length (x)) > (length y)
--                                 then longest' xs x
--                                 else if (length (x)) == (length y)
--                                     then if head x < head y
--                                         then longest' xs x
--                                         else longest' xs y
--                                     else longest' xs y

longest :: Ord a => [[a]] -> [a]
longest l = head (sortBy (comparing (negate . length)) (sortBy (comparing (\x -> head x)) l))

------------------------------------------------------------------------------
-- Ex 6: Implement the function incrementKey, that takes a list of
-- (key,value) pairs, and adds 1 to all the values that have the given key.
--
-- You'll need to add _class constraints_ to the type of incrementKey
-- to make the function work!
--
-- The function needs to be generic and handle all compatible types,
-- see the examples.
--
-- Examples:
--   incrementKey True [(True,1),(False,3),(True,4)] ==> [(True,2),(False,3),(True,5)]
--   incrementKey 'a' [('a',3.4)] ==> [('a',4.4)]

incrementKey :: (Num v, Eq k) => k -> [(k,v)] -> [(k,v)]
incrementKey k l = let f = \(y1, y2) -> if y1 == k 
                                        then (y1, y2 + 1)
                                        else (y1, y2) 
                   in map f l

------------------------------------------------------------------------------
-- Ex 7: compute the average of a list of values of the Fractional
-- class.
--
-- There is no need to handle the empty list case.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
average xs = sum xs / (fromIntegral . length $ xs)

------------------------------------------------------------------------------
-- Ex 8: given a map from player name to score, and two players, return
-- the name of the player with more points. If the players are tied,
-- return the name of the first player (that is, the name of the
-- player who comes first in the argument list, player1).
--
-- If a player doesn't exist in the map, you can assume they have 0 points.
--
-- Hint: Map.findWithDefault can make this simpler
--
-- Examples:
--   winner (Map.fromList [("Bob",3470),("Jane",2130),("Lisa",9448)]) "Jane" "Lisa"
--     ==> "Lisa"
--   winner (Map.fromList [("Mike",13607),("Bob",5899),("Lisa",5899)]) "Lisa" "Bob"
--     ==> "Lisa"

winner :: Map.Map String Int -> String -> String -> String
winner scores player1 player2 = let s1 = score player1
                                    s2 = score player2
                                in if s1 >= s2 then player1 else player2
                                where score p = Map.findWithDefault 0 p scores

------------------------------------------------------------------------------
-- Ex 9: compute how many times each value in the list occurs. Return
-- the frequencies as a Map from value to Int.
--
-- Challenge 1: try using Map.alter for this
--
-- Challenge 2: use foldr to process the list
--
-- Example:
--   freqs [False,False,False,True]
--     ==> Map.fromList [(False,3),(True,1)]

freqs :: (Eq a, Ord a) => [a] -> Map.Map a Int

freqs' [] result = result
freqs' (x:xs) result = case Map.lookup x result of Nothing -> freqs' xs (Map.insert x 1 result)
                                                   Just y  -> freqs' xs (Map.insert x (y+1) result)

freqs'' [] result = result
freqs'' (x:xs) result = let f Nothing = Just 1
                            f (Just y) = Just (y + 1)
                        in freqs'' xs (Map.alter f x result)

freqs = let f x result = case Map.lookup x result of Nothing -> Map.insert x 1 result
                                                     Just y  -> Map.insert x (y+1) result
        in foldr f Map.empty
              

------------------------------------------------------------------------------
-- Ex 10: recall the withdraw example from the course material. Write a
-- similar function, transfer, that transfers money from one account
-- to another.
--
-- However, the function should not perform the transfer if
-- * the from account doesn't exist,
-- * the to account doesn't exist,
-- * the sum is negative,
-- * or the from account doesn't have enough money.
--
-- Hint: there are many ways to implement this logic. Map.member or
-- Map.notMember might help.
--
-- Examples:
--   let bank = Map.fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Mike" 20 bank
--     ==> fromList [("Bob",80),("Mike",70)]
--   transfer "Bob" "Mike" 120 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Bob" "Lisa" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]
--   transfer "Lisa" "Mike" 20 bank
--     ==> fromList [("Bob",100),("Mike",50)]

transfer :: String -> String -> Int -> Map.Map String Int -> Map.Map String Int
transfer from to amount bank = if Map.member from bank && Map.member to bank && amount >= 0 && (Map.findWithDefault 0 from bank) - amount >= 0
                               then Map.alter deposit to (Map.alter withdraw from bank)
                               else bank
                               where deposit (Just x) = Just (x + amount)
                                     deposit Nothing = Nothing
                                     withdraw (Just x) = Just (x - amount)
                                     withdraw Nothing = Nothing

------------------------------------------------------------------------------
-- Ex 11: given an Array and two indices, swap the elements in the indices.
--
-- Example:
--   swap 2 3 (array (1,4) [(1,"one"),(2,"two"),(3,"three"),(4,"four")])
--         ==> array (1,4) [(1,"one"),(2,"three"),(3,"two"),(4,"four")]

swap :: Ix i => i -> i -> Array i a -> Array i a
swap i j arr = arr // [(i, arr ! j), (j, arr ! i)]

------------------------------------------------------------------------------
-- Ex 12: given an Array, find the index of the largest element. You
-- can assume the Array isn't empty.
--
-- You may assume that the largest element is unique.
--
-- Hint: check out Data.Array.indices or Data.Array.assocs

maxIndex :: (Ix i, Ord a) => Array i a -> i
maxIndex arr = let is = indices arr 
               in foldr f (head is) is
               where f = \i ib -> if (arr ! i) > (arr ! ib) then i else ib