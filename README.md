# template-haskell-project

[![Build Status](https://img.shields.io/github/actions/workflow/status/pascalpoizat/template-haskell-project/CI.yml?style=flat-square)](https://github.com/pascalpoizat/template-haskell-project/actions/workflows/CI.yml)
[![License](https://img.shields.io/github/license/pascalpoizat/template-haskell-project.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/github/tag/pascalpoizat/template-haskell-project.svg?label=version&style=flat-square)](template-haskell-project.cabal)<br/>

A simple template for Haskell projects built with Stack, together with some basic information to get into Haskell.

Some complementary language-independent information can be found [here](https://github.com/pascalpoizat/template-java-project) (one may have to dig among Java-specific stuff).

Note: version in the [source code](https://github.com/pascalpoizat/template-haskell-project/blob/master/src/Lib.hs), the [Cabal](https://github.com/pascalpoizat/template-haskell-project/blob/master/template-haskell-project.cabal) configuration, and the [CI workflow](https://github.com/pascalpoizat/template-haskell-project/blob/master/.github/workflows/CI.yml) (this should be automated).

## Installing the environment

The environment is made up of the [GHC compiler](https://www.haskell.org/ghc/) and a dependency manager / build tool such as [Cabal](https://cabal.readthedocs.io/en/stable/) or [Stack](https://docs.haskellstack.org/en/stable/). Haskell libraries can be found on [Hackage](https://hackage.haskell.org). Stack comes with sets of compatible libraries from Hackage organized in [Long Term Support releases](https://www.stackage.org)(LTS).

Versions we use (at the time of writing):

| GHC | Cabal | Stack | LTS |
| ---:|   ---:|   ---:| ---:|
|9.2.5| 3.6.2 | 2.9.1 | 20.5|

Installing the environment is simplified using [GHCup](https://www.haskell.org/ghcup/).

1. install GHCup as [presented here](https://www.haskell.org/ghcup/install/)
2. install GHC, Cabal and/or Stack using GHCup as [presented here](https://www.haskell.org/ghcup/install/).

*This project requires installing both Cabal and Stack.*

## IDE

A simple (and [feature-rich](https://haskell-language-server.readthedocs.io/en/stable/features.html)) solution is to use the [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/stable/) (HLS) which can be installed using GHCup as [presented here](https://haskell-language-server.readthedocs.io/en/stable/installation.html#ghcup), and have your IDE work with it.

For [Visual Studio Code]() you can use the [Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), which is able to download HLS by need for you (indeed, it uses GHCUp for this).

But HLS works with [many more](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor) IDEs.

## Tools

- [summoner](https://github.com/kowainik/summoner), to setup Haskell projects (can avoid a lot of burden when starting a new project)
- [hlint](https://github.com/ndmitchell/hlint) and [stan](https://github.com/kowainik/stan), to analyse your code and/or propose refactoring

## Libraries

### Archives

- [Hackage](https://hackage.haskell.org) in general
- [Stackage](https://www.stackage.org) for use with Stack

### Personal selection

- **specific preludes**
    - [relude](https://hackage.haskell.org/package/relude), lightweight standard library
- **data structures**
    - [containers](https://hackage.haskell.org/package/containers), for assorted container types
- **parsing:**
    - [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative), for parsing command-line options
    - [attoparsec](https://hackage.haskell.org/package/attoparsec), for writing more complex parsers
- **file formats:**
    - [aeson](https://hackage.haskell.org/package/aeson), for JSON parsing/encoding
    - [xml](https://hackage.haskell.org/package/xml), for simple XML-related tasks, and [HaXml](https://hackage.haskell.org/package/HaXml), for more complex ones
    - [cassava](https://hackage.haskell.org/package/cassava), for CSV parsing/encoding