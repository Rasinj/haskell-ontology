{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Ontology.Tree
Description : Tree-based ontology system for hierarchical classifications
Copyright   : (c) Rasinj, 2025
License     : MIT
Maintainer  : rasinj@example.com
Stability   : experimental

This module provides a tree-based data structure for representing ontological
hierarchies and finding common ancestors between nodes.
-}

module Data.Ontology.Tree
    ( -- * Data Types
      Node(..)
    , Level
    , NodeLabel
      -- * Construction
    , mkNode
    , mkLeaf
      -- * Comparison
    , compareNode
      -- * Accessors
    , getLevel
    , getLabel
    , getParent
      -- * Traversal
    , findLowestCommonAncestor
    , findAncestorPath
      -- * Example Data
    , exampleContinuant
    , exampleProcess
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

-- | The depth level in the tree, where 0 is the root
type Level = Int

-- | A label describing the node
type NodeLabel = Text

-- | The prototypical tree structure. A Node contains a label, depth level,
-- and optional parent node. This represents an ontological hierarchy where
-- each node knows its position and ancestry.
data Node
    = Node
        { nodeLabel  :: !NodeLabel  -- ^ The descriptive label for this node
        , nodeLevel  :: !Level      -- ^ The depth from the root (0 = root)
        , nodeParent :: !Node       -- ^ The parent node
        }
    | NodeLeaf
        { nodeLabel :: !NodeLabel   -- ^ The descriptive label for this leaf
        , nodeLevel :: !Level       -- ^ The depth from the root (0 = root)
        }
    | Empty                         -- ^ Represents the absence of a node
    deriving (Eq, Show)

-- | Smart constructor for creating a node with a parent.
-- Automatically sets the level to one more than the parent's level.
mkNode :: NodeLabel -> Node -> Node
mkNode label parent = Node label (getLevel parent + 1) parent

-- | Smart constructor for creating a leaf node at a specific level.
mkLeaf :: NodeLabel -> Level -> Node
mkLeaf = NodeLeaf

-- | Compare two nodes for equality
compareNode :: Node -> Node -> Bool
compareNode a b = a == b

-- | Get the parent node, or Empty if there is no parent
getParent :: Node -> Node
getParent (Node _ _ parent) = parent
getParent (NodeLeaf _ _)    = Empty
getParent Empty             = Empty

-- | Get the level (depth) of a node. Empty nodes are at level 0.
getLevel :: Node -> Level
getLevel (Node _ level _) = level
getLevel (NodeLeaf _ level) = level
getLevel Empty = 0

-- | Get the label of a node. Returns Nothing for Empty nodes.
getLabel :: Node -> Maybe NodeLabel
getLabel (Node label _ _) = Just label
getLabel (NodeLeaf label _) = Just label
getLabel Empty = Nothing

-- | Get the label as Text, with a default for Empty nodes
getLabelOrDefault :: NodeLabel -> Node -> NodeLabel
getLabelOrDefault defaultLabel node = fromMaybe defaultLabel (getLabel node)

-- | Example data for testing: A Continuant entity hierarchy
exampleContinuant :: Node
exampleContinuant = Node "Continuant" 2 (Node "Object" 1 (NodeLeaf "Root" 0))

-- | Example data for testing: A Process entity hierarchy
-- Note: Fixed typo from original "Objict" to "Object"
exampleProcess :: Node
exampleProcess = Node "Process" 2 (Node "Object" 1 (NodeLeaf "Root" 0))

-- | Compare the levels of two nodes
compareLevels :: Node -> Node -> Ordering
compareLevels a b = compare (getLevel a) (getLevel b)

-- | Find the lowest common ancestor of two nodes in the ontology tree.
-- This function traverses up the tree from both nodes until it finds
-- a common ancestor.
--
-- >>> findLowestCommonAncestor exampleContinuant exampleProcess
-- "Object"
findLowestCommonAncestor :: Node -> Node -> NodeLabel
findLowestCommonAncestor a b =
    case compareLevels a b of
        EQ -> case getLabel a == getLabel b of
                True  -> getLabelOrDefault "" a
                False -> findLowestCommonAncestor (getParent a) (getParent b)
        LT -> findLowestCommonAncestor a (getParent b)
        GT -> findLowestCommonAncestor (getParent a) b

-- | Find the path from a node to its lowest common ancestor with another node.
-- Returns a path string with labels separated by forward slashes.
-- The path accumulator starts empty and builds up as we traverse.
--
-- >>> findAncestorPath exampleContinuant exampleProcess
-- "Continuant/Object"
findAncestorPath :: Node -> Node -> Text
findAncestorPath = findAncestorPath' ""
  where
    findAncestorPath' :: NodeLabel -> Node -> Node -> NodeLabel
    findAncestorPath' acc a b =
        case compareLevels a b of
            EQ -> case getLabel a == getLabel b of
                    True  -> buildPath (getLabelOrDefault "" a) acc
                    False -> findAncestorPath' (buildPath (getLabelOrDefault "" a) acc)
                                               (getParent a)
                                               (getParent b)
            LT -> findAncestorPath' (buildPath (getLabelOrDefault "" a) acc)
                                    a
                                    (getParent b)
            GT -> findAncestorPath' (buildPath (getLabelOrDefault "" a) acc)
                                    (getParent a)
                                    b

    buildPath :: NodeLabel -> NodeLabel -> NodeLabel
    buildPath current acc
        | T.null acc = current
        | otherwise  = current <> "/" <> acc
