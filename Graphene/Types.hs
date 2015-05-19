{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Graphene.Types
       ( Drawable(..)
       ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text
import Data.Typeable

class Drawable backend a where
  draw :: a -> Diagram backend 
  
instance Diagram backend ~ dbac => Drawable backend dbac where
  draw = id
instance (Renderable (Text (N b)) b,V b~V2,Typeable (N b),RealFloat (N b))
    => Drawable b String where
  draw = text
