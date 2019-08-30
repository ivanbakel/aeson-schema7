{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE TypeOperators, Rank2Types, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Generics.Traversable.Generic where

import Data.Proxy (Proxy (..))
import Data.Generics.Traversable
import GHC.Generics

class GGTraversable c (f :: * -> *) where
  ggtraverse :: Applicative g => Proxy c -> (forall d. c d => d -> g d) -> f p -> g (f p)

instance GGTraversable c U1 where
  ggtraverse Proxy _f U1 = pure U1

instance GGTraversable c V1 where
  ggtraverse Proxy _f = error "Impossible"

instance (c con) => GGTraversable c (K1 i con) where
  ggtraverse Proxy f (K1 con) = K1 <$> f con

instance (GGTraversable c f) => GGTraversable c (M1 i meta f) where
  ggtraverse proxy f (M1 inner) = M1 <$> ggtraverse proxy f inner

instance (GGTraversable c f, GGTraversable c g) => GGTraversable c (f :+: g) where
  ggtraverse proxy f (L1 val) = L1 <$> ggtraverse proxy f val
  ggtraverse proxy f (R1 val) = R1 <$> ggtraverse proxy f val

instance (GGTraversable c f, GGTraversable c g) => GGTraversable c (f :*: g) where
  ggtraverse proxy f (left :*: right)
    = (:*:) <$> ggtraverse proxy f left <*> ggtraverse proxy f right

instance
    (Generic a, GGTraversable c (Rep a))
  => GTraversable c a
  where
  gtraverse f val
    = to <$> ggtraverse (Proxy @c) f (from val)
