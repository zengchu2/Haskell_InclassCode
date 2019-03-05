module TrieDelete where

import           TrieDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


trieDelete :: [Char] -> Trie a -> Trie a
trieDelete [] tn@(TrieNode value (children)) = (TrieNode Nothing children)
trieDelete (x:xs) tn@(TrieNode value children) 
    | Map.member x children == False = tn
    | otherwise = (TrieNode value (op children)) 
                    where   (Just target) = Map.lookup x children
                            newchild = trieDelete xs (target)
                            op children = if  (trieIsEmpty newchild == True) then (Map.delete x children)
                                          else (Map.insert x newchild children)
