{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleContexts   #-}
module Graphene.ForceLayout(
        graphPos
        ) where

import qualified Data.Vector.Unboxed.Mutable as M 
import qualified Data.Vector.Unboxed as V
import Data.Graph.Inductive.Graph
import Data.Maybe
import Data.List

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import System.Random

import Diagrams.Prelude
import Graphene.Types

import Diagrams.TwoD.Size()

-- position and force type forcetype
type PositionVector s   = M.MVector (PrimState (ST s)) (V2 Double)
type ForceVector s      = M.MVector (PrimState (ST s)) (V2 Double)

-- to get initial random positions
rndNPoints :: Int   --size of random list
           -> [V2 Double] 
rndNPoints n = zipWith (curry r2) (randL 0) (randL 1)
        where
          randL l = take n (randomRs (1,10) (mkStdGen l))

-- function to take graph as input and return graph with 
-- node label as (Diagram,position)
graphPos :: ( DynGraph gr
            , V y ~ V2
            , Enveloped y
            , Drawable b y
            , N y ~ Double
            )
         => gr y c  -- input graph
         -> Int     -- number of iterations to run the algorithm
         -> gr (y,V2 Double) c -- output graph with node label as (Diagram,position)
graphPos graph n = gmap changeGraph graph
        where
            changeGraph (p,q,r,s) = (p,q,(r,outVect V.! (q-1)),s)
            outVect               = nodesPos graph n

-- function to return vector of node positions after layout
nodesPos :: ( DynGraph gr
            , V y ~ V2
            , Enveloped y
            , N y ~ Double
            )
         => gr y c  --input graph 
         -> Int   -- number of iterations
         -> V.Vector (V2 Double)
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
updatePos :: ( PrimMonad (ST s) 
             , DynGraph gr
             , V y~ V2
             , Enveloped y
             , N y ~ Double
             )
          => gr y c  --input graph 
          -> Int 
          -> PositionVector s --Mutable vector containing positions of nodes 
          -> ForceVector s --Mutable vector containing force applied at every node  
          -> ST s ()
updatePos _ 0 _ _               = return ()
updatePos inGraph n pos force   = do 
    forceAtN inGraph pos force
    forM_ (nodes inGraph) $ \i -> do
      v <- M.read force (i-1)
      M.write pos   (i-1) v
      M.write force (i-1) (r2 (0.0,0.0))
    updatePos inGraph (n-1) pos force

-- update force at every node in single iteration
forceAtN :: ( DynGraph gr
            , PrimMonad (ST s)
            , V y~ V2
            , Enveloped y
            , N y ~ Double
            ) 
         => gr y c  --input graph 
         -> PositionVector s --Mutable vector containing positions of nodes 
         -> ForceVector s --Mutable vector containing force applied at every node 
         -> ST s ()
forceAtN g pos force = forM_ [1 .. noNodes g] $
  \ n ->
    do forM_ (neighbors g n) $ 
        \ i -> 
           attForce pos force n i (dynK g n i)
       forM_ (nodes g \\ (neighbors g n ++ [n])) $
         \ j ->
           repForce pos force j n (dynK g n j)

--calculate attractive force at a node        
attForce :: ( PrimMonad (ST s)
            )
         => PositionVector s --Mutable vector containing positions of nodes 
         -> ForceVector s --Mutable vector containing force applied at every node 
         -> Int
         -> Int
         -> Double
         -> ST s ()
attForce pos force n1 n2 dynDis = do
  forcePre <- M.read force (n1-1)
  affected <- calforceAttr dynDis <$> M.read pos (n1-1) <*> M.read pos (n2-1)
  M.write force (n1-1) (affected + forcePre)

-- attractive force between two nodes
calforceAttr :: Double 
             -> V2 Double
             -> V2 Double
             -> V2 Double
calforceAttr dynDis pos1 pos2 = result
  where
    dist    = distanceA pos1 pos2
    d       = if dist /= 0 then dist else dynDis
    effect  = -1*(2*log d)
    result  = (effect/d)*^(pos1-pos2)

--calculate repulsive force on a node
repForce :: ( PrimMonad (ST s)
            )
         => PositionVector s --Mutable vector containing positions of nodes 
         -> ForceVector s --Mutable vector containing force applied at every node 
         -> Int    
         -> Int
         -> Double
         -> ST s ()
repForce pos force n1 n2 dynDis = do
  forcePre <- M.read force (n1-1)
  affected <- calForceRep dynDis <$> M.read pos (n1-1) <*> M.read pos (n2-1)
  M.write force (n1-1) (affected + forcePre)

-- repulsive force between two nodes
calForceRep :: Double
            -> V2 Double
            -> V2 Double
            -> V2 Double
calForceRep dynDis pos1 pos2 = result
  where
    dist    = distanceA pos1 pos2
    d       = if dist /= 0 then dist else dynDis
    effect  = 1/sqrt d
    result  = (effect/d)*^(pos1-pos2)

-- function to calculate dynamic length of spring depending on node sizes
-- here fixed natural length of spring is 5
dynK :: ( DynGraph gr
        , V a ~ V2
        , Enveloped a
        , N a ~ Double
        )
     => gr a c 
     -> Int
     -> Int
     -> Double
dynK g s t = max minK 5
    where
        v1      = fromJust (lab g s)
        v2      = fromJust (lab g t)
        minK    = (l1/2) + (l2/2) + 4
        l1      = diag v1
        l2      = diag v2
        diag v = sqrt (width v * width v + height v * height v)
