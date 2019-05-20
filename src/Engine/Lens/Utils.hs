{-# Language ScopedTypeVariables #-}
{-# Language KindSignatures #-}
{-# Language TypeFamilies #-}
{-# Language ConstraintKinds #-}
{-# Language RankNTypes #-}
module Engine.Lens.Utils where

import Control.Lens
import Data.Generics.Product as HasField (HasField'(field'))
import Data.Generics.Sum.Constructors
import GHC.TypeLits
import GHC.OverloadedLabels

--------------------------------------------------------------------------------

data SymbolProxy (s :: Symbol) = SProxy

instance (s0 ~ s1) => IsLabel (s0 :: Symbol) (SymbolProxy (s1 :: Symbol)) where
    fromLabel = SProxy

type HasF f = HasField' (AppendSymbol "field_" f)

ff :: forall f ff s a. (ff ~ AppendSymbol "field_" f, HasField' ff s a)
    => SymbolProxy f -> Lens' s a
ff SProxy = field' @ff

cc :: forall f ff s a. (ff ~ AppendSymbol "_" f, AsConstructor' f s a)
    => SymbolProxy ff -> Prism' s a
cc SProxy = _Ctor' @f

