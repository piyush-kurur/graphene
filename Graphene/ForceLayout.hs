{-# LANGUAGE TypeFamilies   #-}
module Graphene.ForceLayout(
        graphPos,
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

import Diagrams.Prelude
import Graphene.Types

import Diagrams.TwoD.Size()

-- to get initial random positions
rndNPoints :: Int -> [V2 Double]
rndNPoints n = zipWith (curry r2) (randL 0) (randL 1)
        where
                randL l = take n (randomRs (1,10) (mkStdGen l))

-- function to take graph as input and return graph with 
-- node label as (Diagram,position)
graphPos :: (DynGraph gr,V y ~ V2,Enveloped y,Drawable b y, N y ~ Double) => 
            gr y c -> Int -> gr (y,V2 Double) c
graphPos graph n = gmap changeGraph graph
        where
            changeGraph (p,q,r,s) = (p,q,(r,outVect V.! (q-1)),s)
            outVect      = nodesPos graph n

-- function to return vector of node positions after layout
nodesPos :: (DynGraph gr,V y ~ V2,Enveloped y, N y ~ Double) => 
            gr y c -> Int -> V.Vector (V2 Double)
nodesPos graph n = runST $ do
        pos     <- V.thaw pos1
        force   <- V.thaw for1
        updatePos graph n pos force
        V.freeze pos
        where
            pos1    = V.fromList (rndNPoints (noNodes  graph))
            for1    = V.fromList (nPos (noNodes  graph))
            nPos n1  = replicate n1 (r2 (0.0,0.0))

-- update position for n number of times
updatePos :: (PrimMonad m , DynGraph gr,
                V y~ V2, Enveloped y, N y ~ Double) => 
                gr y c -> Int -> 
                M.MVector (PrimState m) (V2 Double) -> 
                M.MVector (PrimState m) (V2 Double) -> m ()
updatePos _ 0 _ _               = return ()
updatePos inGraph n pos force   = do 
    forceAtN inGraph pos force
    forM_ (nodes inGraph) $ \i -> do
      v <- M.read force (i-1)
      M.write pos (i-1) v
      M.write force (i-1) (r2 (0.0,0.0))
    updatePos inGraph (n-1) pos force

-- update force at every node in single iteration
forceAtN :: (DynGraph gr, PrimMonad m, V y~ V2, Enveloped y, N y ~ Double) => 
            gr y c -> M.MVector (PrimState m) (V2 Double) -> 
            M.MVector (PrimState m) (V2 Double) -> m ()
forceAtN g pos force = forM_ [1 .. noNodes g] $
  \ n ->
    do forM_ (neighbors g n) $ 
        \ i -> 
           attForce pos force n i (dynK (fromJust (lab g n)) (fromJust (lab g i)))
       forM_ (nodes g \\ (neighbors g n ++ [n])) $
         \ j ->
           repForce pos force j n (dynK (fromJust (lab g n)) (fromJust (lab g j)))

--calculate attractive force at a node        
attForce :: (PrimMonad m)=> M.MVector (PrimState m) (V2 Double) -> 
            M.MVector (PrimState m) (V2 Double) -> Int -> Int -> Double -> m ()
attForce pos force n1 n2 dynDis = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + calforceAttr dynDis pos1 pos2)


calforceAttr :: Double -> V2 Double -> V2 Double -> V2 Double
calforceAttr dynDis pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    d       = if dist /= 0 then dist else dynDis
    effect  = -1*(2*log d)
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)

--calculate repulsive force on a node
repForce :: (PrimMonad m)=> M.MVector (PrimState m) (V2 Double) -> 
            M.MVector (PrimState m) (V2 Double) ->  Int -> Int -> Double -> m ()
repForce pos force n1 n2 dynDis = do
  pos1 <- M.read pos (n1-1)
  pos2 <- M.read pos (n2-1)
  forcePre <- M.read force (n1-1)
  M.write force (n1-1) (forcePre + calForceRep dynDis pos1 pos2)

calForceRep :: Double -> V2 Double -> V2 Double -> V2 Double
calForceRep dynDis pos1 pos2 = result
  where
    (x1,y1) = unr2 pos1
    (x2,y2) = unr2 pos2
    dist    = sqrt ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
    d       = if dist /= 0 then dist else dynDis
    effect  = 1/sqrt d
    result  = r2 (effect*(x1-x2)/d,effect*(y1-y2)/d)

-- here fixed natural length of spring is 5
dynK :: (V a ~ V2, Enveloped a, N a ~ Double) => a -> a -> Double
dynK v1 v2 = max (calMinK v1 v2) 5

-- here minimum distance between nodes is 4 units
calMinK :: (V a ~ V2, Enveloped a, N a ~ Double) => a -> a -> Double
calMinK v1 v2 = (l1/2) + (l2/2) + 4
        where
            l1 = diagonal v1
            l2 = diagonal v2

diagonal :: (V a ~ V2, Enveloped a, N a ~ Double) => a -> Double
diagonal v = sqrt (width v * width v + height v * height v)