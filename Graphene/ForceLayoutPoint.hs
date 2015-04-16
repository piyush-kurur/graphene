{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphene.ForceLayoutPoint(
    points
    ,R2) where


import qualified Data.Vector.Unboxed.Mutable as M 
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving
import Data.Graph.Inductive.Graph
import Data.List

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import System.Random

import Diagrams.Prelude

derivingUnbox "R2" [t| R2 → (Double,Double)|] [|unr2|] [|r2|]

zeroPos :: R2
zeroPos = r2 (0.0,0.0)

nPos :: Int -> [R2]
nPos n = replicate n zeroPos

rndNPoints :: Int -> [R2]
rndNPoints size = zipWith toR2 x y
  where
    toR2 p q = r2 (p, q)
    x     = take size single
    y     = drop size single
    single  = take (2*size) (randomRs (1,10) (mkStdGen 0))


points :: (DynGraph gr) => gr y b -> Int -> V.Vector R2
points graph n = runST $ do
        pos     <- V.thaw pos1
        force   <- V.thaw for1
        updatePos graph n pos force
        V.freeze pos
        where
            pos1 = V.fromList (rndNPoints (noNodes  graph))
            for1 = V.fromList (nPos (noNodes  graph))

updatePos :: (PrimMonad m , DynGraph gr) => gr y b -> Int -> 
                M.MVector (PrimState m) R2 -> 
                M.MVector (PrimState m) R2 -> m ()
updatePos _ 0 _ _  = return ()
updatePos inGraph n pos force  = do 
    forceAtN inGraph pos force
    forM_ (nodes inGraph) $ \i -> do
      v <- M.read force (i-1)
      M.write pos (i-1) v
      M.write force (i-1) zeroPos
    updatePos inGraph (n-1) pos force

forceAtN :: ( DynGraph gr , PrimMonad m ) => gr y b -> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> m ()
forceAtN g pos force = forM_ [1 .. noNodes g] $
  \ n ->
    do forM_ (neighbors g n) $ 
        \ i -> 
           attForce pos force n i
       forM_ (nodes g \\ (neighbors g n ++ [n])) $
         \ j ->
           repForce pos force j n
        
attForce :: (PrimMonad m)=> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> 
            Int -> Int -> m ()
attForce pos force n1 n2 = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + calforceAttr pos1 pos2)

calforceAttr :: R2 -> R2 -> R2
calforceAttr pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    -- if two nodes overlap then we take force according to atleast 5 unit separation 
    d       = if dist /= 0 then dist else 5
    effect  = -1 * (2 * log d)
--    effect  = if typeOf == 1 then (-1 * (d / dynDis)) else (dynDis / d) * (dynDis / d)
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)

repForce :: (PrimMonad m)=> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> 
            Int -> Int -> m ()
repForce pos force n1 n2 = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + calForceRep pos1 pos2)

calForceRep :: R2 -> R2 -> R2
calForceRep pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    -- if two nodes overlap then we take force according to atleast 5 unit separation 
    d       = if dist /= 0 then dist else 5  
    effect  = 1 / sqrt d
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)