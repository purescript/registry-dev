module Foreign.Graph (Graph, Node, Edge, mkGraph, topsort) where

import Data.Function.Uncurried (Fn1, Fn2)
import Data.Function.Uncurried as Fn

foreign import data Graph :: Type -> Type -> Type

foreign import createImpl
  :: forall k v
   . Fn2 (Array (Node k v)) (Array (Edge k)) (Graph k v)

foreign import topsortImpl :: forall k v. Fn1 (Graph k v) (Array k)

type Node k v = { id :: k, v :: v }

type Edge k = { from :: k, to :: k }

mkGraph :: forall k v. Array (Node k v) -> Array (Edge k) -> Graph k v
mkGraph = Fn.runFn2 createImpl

topsort :: forall k v. Graph k v -> Array k
topsort = Fn.runFn1 topsortImpl
