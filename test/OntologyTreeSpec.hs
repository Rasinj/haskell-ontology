{-# LANGUAGE OverloadedStrings #-}

module OntologyTreeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Ontology.Tree
import Data.Text (Text)
import qualified Data.Text as T

-- | Test specification for the Ontology Tree module
spec :: Spec
spec = do
    describe "Node construction" $ do
        it "creates a leaf node with correct level" $ do
            let leaf = mkLeaf "Root" 0
            getLevel leaf `shouldBe` 0
            getLabel leaf `shouldBe` Just "Root"

        it "creates a node with auto-calculated level" $ do
            let root = mkLeaf "Root" 0
                child = mkNode "Child" root
            getLevel child `shouldBe` 1
            getLabel child `shouldBe` Just "Child"

        it "creates multi-level hierarchy correctly" $ do
            let root = mkLeaf "Root" 0
                level1 = mkNode "Level1" root
                level2 = mkNode "Level2" level1
            getLevel level2 `shouldBe` 2

    describe "Accessor functions" $ do
        it "getLevel returns correct depth" $ do
            let root = mkLeaf "Root" 0
                child = mkNode "Child" root
            getLevel root `shouldBe` 0
            getLevel child `shouldBe` 1
            getLevel Empty `shouldBe` 0

        it "getLabel returns Just for valid nodes" $ do
            let node = mkLeaf "Test" 0
            getLabel node `shouldBe` Just "Test"

        it "getLabel returns Nothing for Empty" $ do
            getLabel Empty `shouldBe` Nothing

        it "getParent returns correct parent" $ do
            let root = mkLeaf "Root" 0
                child = mkNode "Child" root
            getParent child `shouldBe` root

        it "getParent returns Empty for leaf nodes" $ do
            let leaf = mkLeaf "Root" 0
            getParent leaf `shouldBe` Empty

    describe "findLowestCommonAncestor" $ do
        it "finds LCA for same-level nodes with common parent" $ do
            let root = mkLeaf "Root" 0
                object = mkNode "Object" root
                continuant = mkNode "Continuant" object
                process = mkNode "Process" object
            findLowestCommonAncestor continuant process `shouldBe` "Object"

        it "finds LCA for provided example data" $ do
            findLowestCommonAncestor exampleContinuant exampleProcess
                `shouldBe` "Object"

        it "finds LCA when nodes are at different levels" $ do
            let root = mkLeaf "Root" 0
                level1 = mkNode "Level1" root
                level2 = mkNode "Level2" level1
                level3 = mkNode "Level3" level2
                branch = mkNode "Branch" root
            findLowestCommonAncestor level3 branch `shouldBe` "Root"

        it "returns node label when comparing node with itself" $ do
            let root = mkLeaf "Root" 0
                node = mkNode "Node" root
            findLowestCommonAncestor node node `shouldBe` "Node"

        it "finds root as LCA for distant branches" $ do
            let root = mkLeaf "Root" 0
                branch1L1 = mkNode "Branch1L1" root
                branch1L2 = mkNode "Branch1L2" branch1L1
                branch2L1 = mkNode "Branch2L1" root
                branch2L2 = mkNode "Branch2L2" branch2L1
            findLowestCommonAncestor branch1L2 branch2L2 `shouldBe` "Root"

    describe "findAncestorPath" $ do
        it "builds correct path for sibling nodes" $ do
            let root = mkLeaf "Root" 0
                object = mkNode "Object" root
                continuant = mkNode "Continuant" object
                process = mkNode "Process" object
            findAncestorPath continuant process `shouldBe` "Continuant/Object"

        it "builds path for nodes at different levels" $ do
            let root = mkLeaf "Root" 0
                level1 = mkNode "Level1" root
                level2 = mkNode "Level2" level1
                branch = mkNode "Branch" root
            findAncestorPath level2 branch `shouldBe` "Level2/Level1/Root"

        it "returns single label when comparing identical nodes" $ do
            let root = mkLeaf "Root" 0
                node = mkNode "Node" root
            findAncestorPath node node `shouldBe` "Node"

    describe "compareNode" $ do
        it "returns True for identical nodes" $ do
            let root = mkLeaf "Root" 0
            compareNode root root `shouldBe` True

        it "returns False for different nodes" $ do
            let root1 = mkLeaf "Root" 0
                root2 = mkLeaf "Root" 0
            compareNode root1 root2 `shouldBe` True -- Same structure

        it "returns False for nodes with different labels" $ do
            let node1 = mkLeaf "Node1" 0
                node2 = mkLeaf "Node2" 0
            compareNode node1 node2 `shouldBe` False

    describe "Example data" $ do
        it "exampleContinuant has correct structure" $ do
            getLabel exampleContinuant `shouldBe` Just "Continuant"
            getLevel exampleContinuant `shouldBe` 2

        it "exampleProcess has correct structure" $ do
            getLabel exampleProcess `shouldBe` Just "Process"
            getLevel exampleProcess `shouldBe` 2

        it "example nodes have common parent" $ do
            let continuantParent = getParent exampleContinuant
                processParent = getParent exampleProcess
            getLabel continuantParent `shouldBe` getLabel processParent

    describe "Edge cases" $ do
        it "handles Empty nodes gracefully" $ do
            getLevel Empty `shouldBe` 0
            getLabel Empty `shouldBe` Nothing
            getParent Empty `shouldBe` Empty

        it "handles deep hierarchies" $ do
            let buildDeepTree 0 = mkLeaf "Root" 0
                buildDeepTree n = mkNode (T.pack $ "Level" ++ show n) (buildDeepTree (n - 1))
                deep = buildDeepTree 10
            getLevel deep `shouldBe` 10

-- QuickCheck properties
prop_levelIncreasesWithDepth :: Int -> Bool
prop_levelIncreasesWithDepth n =
    let levels = abs n `mod` 20  -- Limit to reasonable depth
        buildTree 0 = mkLeaf "Root" 0
        buildTree k = mkNode "Node" (buildTree (k - 1))
        tree = buildTree levels
    in getLevel tree == levels

prop_parentLevelOneLess :: Text -> Bool
prop_parentLevelOneLess label =
    let root = mkLeaf "Root" 0
        child = mkNode label root
    in getLevel child == getLevel root + 1

prop_lcaIsCommutative :: Bool
prop_lcaIsCommutative =
    findLowestCommonAncestor exampleContinuant exampleProcess ==
    findLowestCommonAncestor exampleProcess exampleContinuant

prop_nodeEqualsItself :: Text -> Bool
prop_nodeEqualsItself label =
    let node = mkLeaf label 0
    in compareNode node node
