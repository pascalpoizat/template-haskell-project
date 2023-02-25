# template-haskell-project

A simple template for Haskell projects built with Stack 

[![Build Status](https://img.shields.io/github/actions/workflow/status/pascalpoizat/template-haskell-project/CI.yml?style=flat-square)](https://github.com/pascalpoizat/template-haskell-project/actions/workflows/CI.yml)
[![License](https://img.shields.io/github/license/pascalpoizat/template-haskell-project.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/github/tag/pascalpoizat/template-haskell-project.svg?label=version&style=flat-square)](template-haskell-project.cabal)<br/>

- documentation to be updated (meanwhile, it has been removed, see earlier versions if needed).

- some language-independent information can be found here: [template-java-project](https://github.com/pascalpoizat/template-java-project).

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

A simple (and feature-rich) solution is to use the [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/stable/) (HLS) which can be installed using GHCup as [presented here](https://haskell-language-server.readthedocs.io/en/stable/installation.html#ghcup), and have your IDE work with it.

For [Visual Studio Code]() you can use the [Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), which is able to download HLS by need for you (indeed, it uses GHCUp for this).