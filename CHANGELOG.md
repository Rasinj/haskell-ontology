# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2025-11-09

### Added

- Initial release of haskell-ontology library
- Core `Node` data type for representing ontological hierarchies
- Smart constructors `mkNode` and `mkLeaf` for type-safe tree construction
- Accessor functions: `getLevel`, `getLabel`, `getParent`
- `findLowestCommonAncestor` function for finding LCA between two nodes
- `findAncestorPath` function for computing ancestor paths
- Comprehensive Haddock documentation
- Example data for testing (`exampleContinuant`, `exampleProcess`)
- Complete test suite with HSpec and QuickCheck
- GitHub Actions CI/CD pipeline
- MIT License

### Changed

- Migrated from String to Text for better performance and Unicode support
- Refactored code with proper type signatures
- Improved naming conventions (camelCase throughout)
- Enhanced documentation with examples and usage guides

### Fixed

- Fixed typo in example data ("Objict" -> "Object")
- Fixed partial function `getString` to return `Maybe` type
- Improved type safety by handling Empty node cases properly

[0.1.0.0]: https://github.com/Rasinj/haskell-ontology/releases/tag/v0.1.0.0
