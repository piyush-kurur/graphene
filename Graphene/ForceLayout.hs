{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphene.ForceLayout(
    nodesPos) where


import qualified Data.Vector.Unboxed.Mutable as M 
import qualified Data.Vector.Unboxed as V
import Data.Graph.Inductive.Graph
import Data.Maybe
import Data.List

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import System.Random

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Graphene.Types
import Graphene.ForceLayoutPoint()

-- derivingUnbox "R2" [t| R2 → (Double,Double)|] [|unr2|] [|r2|]

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


nodesPos :: (DynGraph gr,Drawable B y, V y~ R2,Enveloped y) => 
            gr y b -> Int -> V.Vector R2
nodesPos graph n = runST $ do
        pos     <- V.thaw pos1
        force   <- V.thaw for1
        updatePos graph n pos force
        V.freeze pos
        where
            pos1 = V.fromList (rndNPoints (noNodes  graph))
            for1 = V.fromList (nPos (noNodes  graph))

updatePos :: (PrimMonad m , DynGraph gr,
                Drawable B y, V y~ R2, Enveloped y) => 
                gr y b -> Int -> 
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

forceAtN :: ( DynGraph gr , PrimMonad m, 
                Drawable B y, V y~ R2,Enveloped y) => gr y b -> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> m ()
forceAtN g pos force = forM_ [1 .. noNodes g] $
  \ n ->
    do forM_ (neighbors g n) $ 
        \ i -> 
           attForce pos force n i (dynK (fromJust (lab g n)) (fromJust (lab g i)))
       forM_ (nodes g \\ (neighbors g n ++ [n])) $
         \ j ->
           repForce pos force j n (dynK (fromJust (lab g n)) (fromJust (lab g j)))
        
attForce :: (PrimMonad m)=> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> 
            Int -> Int -> Double -> m ()
attForce pos force n1 n2 dynDis = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + (calforceAttr dynDis pos1 pos2))

calforceAttr :: Double -> R2 -> R2 -> R2
calforceAttr dynDis pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    d       = if dist /= 0 then dist else dynDis
    effect  = (-1*(2*log d))
--    effect  = if typeOf == 1 then (-1 * (d / dynDis)) else (dynDis / d) * (dynDis / d)
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)

repForce :: (PrimMonad m)=> 
            M.MVector (PrimState m) R2 -> 
            M.MVector (PrimState m) R2 -> 
            Int -> Int -> Double -> m ()
repForce pos force n1 n2 dynDis = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + (calForceRep dynDis pos1 pos2))

calForceRep :: Double -> R2 -> R2 -> R2
calForceRep dynDis pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    d       = if dist /= 0 then dist else dynDis
    effect  = (1/sqrt d)
--    effect  = if typeOf == 1 then (-1 * (d / dynDis)) else (dynDis / d) * (dynDis / d)
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)

-- here fixed natural length of spring is 5
dynK :: (Drawable B a, V a~ R2, Enveloped a) => a -> a -> Double
dynK v1 v2 = max (calMinK v1 v2) 5

-- here minimum distance between nodes is 2 units
calMinK :: (Drawable B a, V a~ R2, Enveloped a) => a -> a -> Double
calMinK v1 v2 = (l1/2) + (l2/2) + 4
        where
                l1 = diagonal v1
                l2 = diagonal v2

diagonal :: (Drawable B a, V a~ R2, Enveloped a) => a -> Double
diagonal v = sqrt(width v * (width v)+ height v * (height v))