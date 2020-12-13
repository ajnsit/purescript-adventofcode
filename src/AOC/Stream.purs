module AOC.Stream where

import Data.Array as A
import Data.CommutativeRing (class Semiring, (+))
import Data.Functor (class Functor)
import Data.Lazy (Lazy, defer, force)

data Stream a = Stream a (Lazy (Stream a))
derive instance functorStream :: Functor Stream

inf :: forall n. Semiring n => n -> n -> Stream n
inf start offset = Stream start (defer \_ -> inf (start+offset) offset)

filter :: forall a. (a -> Boolean) -> Stream a -> Stream a
filter f (Stream a rest) =
  let rest' = defer \_ -> filter f (force rest)
  in if f a then Stream a rest' else force rest'

find :: forall a. (a -> Boolean) -> Stream a -> a
find f (Stream a rest) = if f a then a else find f (force rest)

toArray :: forall a. Stream a -> Array a
toArray (Stream a rest) = A.cons a (toArray (force rest))
