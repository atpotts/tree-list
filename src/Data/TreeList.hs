{-# LANGUAGE FlexibleInstances #-}

module Data.TreeList
 (TreeList(..), TreeListMap, TreeListData(..)
 ) where

import ClassyPrelude
import Data.Map (Map)
import qualified Data.Map.Ordered as OMap

data TreeListMap container name payload = TreeListMap
  { treeListMapPayload :: payload
  , treeListMapChildren :: container name (TreeListMap container name payload)
  }

deriving instance (Show payload, forall a . Show a => Show (container name a)) => Show (TreeListMap container name payload)

class TreeListData x where
  type TreeListKey x :: *
  type TreeListPayload x :: *

instance TreeListData (TreeListMap x n m) where
  type TreeListKey (TreeListMap x n m) = n
  type TreeListPayload (TreeListMap x n m) = m

class (TreeListData x, Monoid x) => TreeList x where
  payload :: x -> TreeListPayload x
  default payload :: (x ~ TreeListMap map n m, m ~ TreeListPayload x) => x -> TreeListPayload x
  payload = treeListMapPayload

  children :: x -> [(TreeListKey x, x)]
  path :: forall fx. (MonoFoldable fx, TreeListKey x ~ Element fx)
    => fx -> TreeListPayload x -> x

instance (Ord n, Semigroup m) => Semigroup (TreeListMap Map n m) where
  TreeListMap m b <> TreeListMap m' b' = TreeListMap (m <> m') (unionWith (<>) b b')

instance (Ord n, Monoid m) => Semigroup (TreeListMap OMap.OMap n m) where
  TreeListMap m b <> TreeListMap m' b' = TreeListMap (m <> m') (OMap.unionWithL (\_ -> (<>)) b b')

instance (Ord n, Monoid m) => Monoid (TreeListMap Map n m) where
  mempty = TreeListMap mempty mempty

instance (Ord n, Monoid m) => Monoid (TreeListMap OMap.OMap n m) where
  mempty = TreeListMap mempty OMap.empty

instance (Ord n, Monoid m) => TreeList (TreeListMap Map n m) where
  children = mapToList . treeListMapChildren
  path pth pl = foldr
    (\p -> TreeListMap mempty . singletonMap p)
    (TreeListMap pl mempty)
    pth

instance (Ord n, Monoid m) => TreeList (TreeListMap OMap.OMap n m) where
  children = OMap.assocs . treeListMapChildren
  path pth pl = foldr
    (\p -> TreeListMap mempty . curry OMap.singleton p)
    (TreeListMap pl OMap.empty)
    pth
