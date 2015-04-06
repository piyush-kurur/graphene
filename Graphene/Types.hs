{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Graphene.Types
       ( Drawable(..)
       ) where

import Diagrams.Prelude
import Diagrams.TwoD.Text

class Drawable backend a where
  draw :: a -> Diagram backend R2

instance Drawable backend (Diagram backend R2) where
  draw = id
instance Renderable Text backend => Drawable backend String where
  draw = text
