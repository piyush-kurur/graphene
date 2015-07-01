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

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad
import System.Random

import Diagrams.Prelude
import Graphene.Types

import Diagrams.TwoD.Size()

-- position and force type forcetype
type PositionVector s   = M.MVector (PrimState (ST s)) (V2 Double)

-- to get initial random positions
rndNPoints :: Int   --size of random 
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
        forM_ [1..n] $ \_ -> iterate1 pos (iterFun graph)
        V.freeze pos
        where
            pos1    = V.fromList (rndNPoints (noNodes  graph))

iterate1 :: (PrimMonad (ST s))
            => PositionVector s
            -> (V.Vector (V2 Double) -> Int -> V2 Double)
            -> ST s ()
iterate1 pos iter = 
    forM_ [0 .. M.length pos - 1] $ \i -> do
        posV    <- V.unsafeFreeze pos
        prePos  <- M.read pos i
        M.write pos i (prePos + 0.1 * (iter posV i))

iterFun :: (DynGraph gr
            , V a ~ V2
            , Enveloped a
            , N a ~ Double
            )
            => gr a b
            -> V.Vector (V2 Double)
            -> Int
            -> V2 Double
iterFun graph pos l = totalF
    where
        totalF  = foldr (+) (r2 (0.0,0.0)) (attr ++ repul)
        attr    = map (cal calforceAttr) neighb
        repul   = map (cal calForceRep) nonNb
        nonNb   = nodes graph \\ (neighb ++ [l+1])
        neighb  = neighbors graph (l+1)
        cal f x = f (dynK graph (l+1) x) (pos V.! l) (pos V.! (x-1))

-- dynamic separation between two nodes
dynK :: ( DynGraph gr
        , V a ~ V2
        , Enveloped a
        , N a ~ Double
        )
     => gr a c 
     -> Int
     -> Int
     -> Double
dynK g s t = max minK 2
    where
        v1      = fromJust (lab g s)
        v2      = fromJust (lab g t)
        minK    = (l1/2) + (l2/2) + 4
        l1      = diag v1
        l2      = diag v2
        diag v = sqrt (width v * width v + height v * height v)

--attractive force between two nodes
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

--repulsive force between two nodes
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
