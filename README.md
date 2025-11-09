# haskell-ontology

A Haskell library for representing hierarchical ontologies as tree structures and finding common ancestors between nodes.

## Overview

`haskell-ontology` provides a simple yet powerful way to model ontological hierarchies (such as taxonomies, classification systems, or organizational structures) and perform queries on them. The library is particularly useful for finding relationships between entities in a hierarchical system.

## Features

- **Tree-based ontology representation** with automatic depth tracking
- **Lowest Common Ancestor (LCA) finding** between any two nodes
- **Ancestor path computation** showing the full path from a node to its common ancestor
- **Type-safe API** with smart constructors to prevent invalid tree construction
- **Text-based labels** for better performance and Unicode support

## Installation

### Using Cabal

Add `haskell-ontology` to your project's `.cabal` file:

```cabal
build-depends:
    base >= 4.14 && < 5
  , haskell-ontology
```

Then build your project:

```bash
cabal build
```

### From Source

Clone the repository and build:

```bash
git clone https://github.com/Rasinj/haskell-ontology.git
cd haskell-ontology
cabal build
cabal test
```

## Usage

### Basic Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Ontology.Tree

-- Create a simple hierarchy: Root -> Object -> Continuant
root :: Node
root = mkLeaf "Root" 0

object :: Node
object = mkNode "Object" root

continuant :: Node
continuant = mkNode "Continuant" object

-- Create another branch: Root -> Object -> Process
process :: Node
process = mkNode "Process" object

-- Find the lowest common ancestor
main :: IO ()
main = do
    print $ findLowestCommonAncestor continuant process
    -- Output: "Object"

    print $ findAncestorPath continuant process
    -- Output: "Continuant/Object"
```

### Ontology Example

Here's a more complete example modeling a basic upper ontology:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Ontology.Tree

-- Build an ontology hierarchy
buildOntology :: Node
buildOntology =
    let root = mkLeaf "Entity" 0
        continuant = mkNode "Continuant" root
        occurrent = mkNode "Occurrent" root

        -- Continuant subtypes
        object = mkNode "Object" continuant
        quality = mkNode "Quality" continuant

        -- Occurrent subtypes
        process = mkNode "Process" occurrent
        event = mkNode "Event" occurrent

    in object

-- Query relationships
queryRelationships :: IO ()
queryRelationships = do
    let root = mkLeaf "Entity" 0
        continuant = mkNode "Continuant" root
        object = mkNode "Object" continuant

        occurrent = mkNode "Occurrent" root
        process = mkNode "Process" occurrent

    putStrLn $ "Common ancestor of Object and Process: "
            ++ show (findLowestCommonAncestor object process)
    -- Output: "Entity"
```

## API Documentation

### Core Types

- **`Node`**: The main data type representing a node in the ontology tree
  - `Node`: An internal node with a label, level, and parent
  - `NodeLeaf`: A leaf node (typically the root) with a label and level
  - `Empty`: Represents the absence of a node

- **`Level`**: Type alias for `Int`, representing depth in the tree (0 = root)
- **`NodeLabel`**: Type alias for `Text`, the descriptive label for a node

### Construction Functions

- **`mkNode :: NodeLabel -> Node -> Node`**: Create a node with a parent (level auto-calculated)
- **`mkLeaf :: NodeLabel -> Level -> Node`**: Create a leaf node at a specific level

### Accessor Functions

- **`getLevel :: Node -> Level`**: Get the depth of a node from the root
- **`getLabel :: Node -> Maybe NodeLabel`**: Get the label (returns `Nothing` for `Empty`)
- **`getParent :: Node -> Node`**: Get the parent node (returns `Empty` if none)

### Query Functions

- **`findLowestCommonAncestor :: Node -> Node -> NodeLabel`**: Find the LCA of two nodes
- **`findAncestorPath :: Node -> Node -> Text`**: Get the path from a node to its LCA

## Development

### Building

```bash
cabal build
```

### Running Tests

```bash
cabal test
```

### Running Tests with Coverage

```bash
cabal test --enable-coverage
```

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Guidelines

1. Follow the existing code style
2. Add tests for new functionality
3. Update documentation as needed
4. Ensure all tests pass before submitting

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Future Enhancements

Potential improvements for future versions:

- [ ] Support for multiple inheritance (DAG structure instead of tree)
- [ ] Serialization/deserialization (JSON, YAML)
- [ ] Distance metrics between nodes
- [ ] Visualization tools
- [ ] Integration with RDF/OWL ontologies
- [ ] Performance optimizations for large ontologies
- [ ] QuickCheck property tests

## References

- [Basic Formal Ontology (BFO)](https://basic-formal-ontology.org/)
- [Web Ontology Language (OWL)](https://www.w3.org/OWL/)

## Authors

- **Rasinj** - Initial work

## Acknowledgments

- Inspired by formal ontology systems and taxonomic hierarchies
- Built with modern Haskell best practices
