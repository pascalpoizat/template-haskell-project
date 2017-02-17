# template-haskell-project

[![Build Status](https://img.shields.io/travis/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://travis-ci.org/pascalpoizat/template-haskell-project)
[![Code Coverage](https://img.shields.io/coveralls/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://coveralls.io/github/pascalpoizat/template-haskell-project)
[![License](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.1.0.0-orange.svg?style=flat-square&label=version)](template-haskell-project.cabal)<br/>
[![Issues Ready](https://img.shields.io/github/issues-raw/pascalpoizat/template-haskell-project/ready.svg?style=flat-square&label=issues%20ready%20for%20development)](https://waffle.io/pascalpoizat/template-haskell-project)
[![Issues in Progress](https://img.shields.io/github/issues-raw/pascalpoizat/template-haskell-project/in%20progress.svg?style=flat-square&label=issues%20in%20progress)](https://waffle.io/pascalpoizat/template-haskell-project)

<!--
[![Version](https://img.shields.io/hackage/v/template-haskell-project.svg?label=version&amp;style=flat-square)](https://hackage.haskell.org/package/template-haskell-project)
-->

This is a simple template for Haskell projects. The first step to create your project is:

```
stack new projectname
```

Then you can clone this project in it:

```
git clone https://github.com/pascalpoizat/template-haskell-project.git projectname
```

You can run your project with:

```
stack exec template-haskell-project-exe
```

You can run your tests (and build the documentation + coverage information) with:

```
stack clean; stack test :template-haskell-project-test  --coverage --haddock --no-haddock-deps
```

## Haskell notes

These are some interesting references related either to learning Haskell or that have been used to set up this template.

### Documentation

- [Learn you a Haskell for great good!](http://learnyouahaskell.com) #1 to read 
- [Haskell Programming from first principles (Haskell Book)](http://haskellbook.com) #2 to read
- [Haskell Wiki](https://wiki.haskell.org/FAQ)
- [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)

### Libraries

- [Hackage central package archive](https://hackage.haskell.org)
- [containers package](https://hackage.haskell.org/package/containers) for various containers such as Set
- [fgl package](http://hackage.haskell.org/package/fgl) for (inductive) graph structures

### Testing

- [Tasty](http://documentup.com/feuerbach/tasty) a testing framework that enables one to combine different kinds of testing (typically, the ones below)
- [HUnit](https://github.com/hspec/HUnit#readme) for unit testing
- [SmallCheck](https://github.com/feuerbach/smallcheck#readme) for property based testing
- [QuickCheck](https://github.com/nick8325/quickcheck#readme) for random based testing
- [HSpec](http://hspec.github.io) BDD testing

### Development

- [Stack](https://haskellstack.org/) build system
- [Cabal](https://www.haskell.org/cabal/) used by Stack
- [Travis CI](https://travis-ci.org) for continuous integration
- [Stack and Travis CI](https://docs.haskellstack.org/en/latest/travis_ci/) for continuous integration
- [HPC](https://wiki.haskell.org/Haskell_program_coverage) for code coverage
- [Coveralls](https://coveralls.io) for code coverage
- [stack-hpc-coveralls](https://github.com/rubik/stack-hpc-coveralls) for code coverage

### IDE

- [Haskell layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell) for [Spacemacs](http://spacemacs.org)

### TODO

see: [issues](https://github.com/pascalpoizat/template-haskell-project/issues)

## Acknowledgements

### For help in installing / running Haskell:

This includes Haskell, Stack, and Spacemacs

[@AkiiZedd (twitter)](https://twitter.com/AkiiZedd),
[@BeRewt (twitter)](https://twitter.com/BeRewt),
[@kaddourkardio (twitter)](https://twitter.com/kaddourkardio), 
[@SergeStinckwich (twitter)](https://twitter.com/SergeStinckwich),
[@spacemacs (twitter)](https://twitter.com/spacemacs)
