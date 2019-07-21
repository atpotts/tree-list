{-# LANGUAGE NoImplicitPrelude
           , TypeFamilies
           , OverloadedStrings
           , ConstraintKinds
           , NoMonomorphismRestriction
#-}

module Main where

import ClassyPrelude
import Data.Function ((&))
import Data.Map (Map)
import Data.MonoTraversable (Element)
import Data.Monoid (All(..))

main :: IO ()
main = return ()

data Ord n => Trie n x = Trie {
  value :: Maybe x,
  branches :: Map n (Trie n x)
} deriving (Read, Show, Eq, Ord)

instance (Ord n, Semigroup x) => Semigroup (Trie n x) where
  Trie m b <> Trie m' b' = Trie (m <> m') (unionWith (<>) b b')

instance (Ord n, Monoid x) => Monoid (Trie n x) where
  mempty = Trie mempty mempty

singletonTrie :: (Ord n, MonoFoldable fn, n ~ Element fn)
  => fn -> x -> Trie n x
singletonTrie path x =
  foldr (\p -> Trie Nothing . singletonMap p) (Trie (Just x) mempty) path

parseTrie ::
  (Ord xs, Eq x,
   IsSequence xs, x ~ Element xs,
   MonoFoldable fxs, xs ~ Element fxs)
  => x -> fxs -> Trie xs All
parseTrie sep rows =
  concatMap (flip singletonTrie (All True) . splitElem sep) rows

type Textish txt = (Ord txt, IsString txt, Textual txt)

type Prefixer x = [x] -> [x]

firstBlocks, lastBlock :: Textish txt => Prefixer txt

lastBlock (x:xs) = ("└── "++x) : map ("    "++) xs
lastBlock [] = []

firstBlocks (x:xs) = ("├── "++x) : map ("│   "++) xs
firstBlocks [] = []

prefixBlocks :: Textish txt => Prefixer [txt]
prefixBlocks [] = []
prefixBlocks [x] = [lastBlock x]
prefixBlocks (x:xs) = firstBlocks x : prefixBlocks xs

showPaths :: Textish txt => Trie txt x -> txt
showPaths = unlines . sp
  where sp (Trie _ children) = children
                             & mapToList
                             & fmap (\(name, value) -> name : sp value)
                             & prefixBlocks
                             & concat
