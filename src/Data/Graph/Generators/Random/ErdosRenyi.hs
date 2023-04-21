-- |
--  Implementations of binomially random graphs, as described by Erdős and Rényi.
--
--  Graphs generated using this method have a constant edge probability between two nodes.
--
--  See Erdős and A. Rényi, On Random Graphs, Publ. Math. 6, 290 (1959).
--
--  graph-generators copyright:
--    Copyright (C) 2014 Uli Köhler
--
--  NetworkX copyright:
--    Copyright (C) 2004-2010 by
--    Aric Hagberg <hagberg@lanl.gov>
--    Dan Schult <dschult@colgate.edu>
--    Pieter Swart <swart@lanl.gov>
--    All rights reserved.
--    BSD license.
module Data.Graph.Generators.Random.ErdosRenyi
  ( -- ** Graph generators
    erdosRenyiGraph,

    -- ** Graph component generators
    erdosRenyiContext,

    -- ** Utility functions
    selectWithProbability,
  )
where

import Data.Graph.Generators
  ( GraphContext (GraphContext),
    GraphInfo (GraphInfo),
  )
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))
import Control.Monad (replicateM)

-- |
--    Generate a unlabelled context using the Erdős and Rényi method.
--
--    See 'erdosRenyiGraph' for a detailed algorithm description.
--
erdosRenyiContext ::
  StatefulGen g m =>
  -- | Identifier of the context's central node
  Int ->
  -- | The algorithm will generate random edges to those nodes
  --   from or to the given node
  [Int] ->
  -- | The probability for any pair of nodes to be connected
  Double ->
  -- | The random number generator to use
  g ->
  -- | The resulting graph (IO required for randomness)
  m GraphContext
erdosRenyiContext n allNodes p gen = do
  endpoints <- selectWithProbability p allNodes gen
  return (GraphContext endpoints n endpoints)

-- |
--    Generate a unlabelled directed random graph using the Algorithm introduced by
--    Erdős and Rényi, also called a binomial random graph generator.
--
--    Note that self-loops with also be generated with probability p.
--
--    This algorithm runs in O(n²) and is best suited for non-sparse networks.
--
--    The generated nodes are identified by [0..n-1].
erdosRenyiGraph ::
  StatefulGen g m =>
  -- | The number of nodes
  Int ->
  -- | The probability for any pair of nodes to be connected
  Double ->
  -- | The random number generator to use
  g ->
  -- | The resulting graph (IO required for randomness)
  m GraphInfo
erdosRenyiGraph n p gen = do
  let allNodes = [0 .. n - 1]
  -- Outgoing edge targets for any node
  outgoingEdgeTargets <- selectWithProbability p allNodes gen
  -- Outgoing edge tuples for a single nodes
  let singleNodeEdges node = zip (repeat node) outgoingEdgeTargets
  let allEdges = concatMap singleNodeEdges allNodes
  return (GraphInfo n allEdges)


-- |
--    Filter a list by selecting each list element
--    uniformly with a given probability p
--
--    Although this is mainly used internally, it can be used as general utility function
selectWithProbability ::
  StatefulGen g m =>
  -- | The probability to select each list element
  Double ->
  -- | The list to filter
  [a] ->
  -- | The random generator state
  g ->
  -- | The filtered list
  m [a]
selectWithProbability p l gen = do
  gl <- replicateM (length l) (uniformRM (0.0, 1.0) gen)
  let ll = filter (\(_, x) -> x < p) (zip l gl)
  return (fst <$> ll)