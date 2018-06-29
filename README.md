# template-haskell-project

[![Build Status](https://img.shields.io/travis/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://travis-ci.org/pascalpoizat/template-haskell-project)
[![Code Coverage](https://img.shields.io/coveralls/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://coveralls.io/github/pascalpoizat/template-haskell-project)
[![License](https://img.shields.io/github/license/pascalpoizat/template-haskell-project.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/github/tag/pascalpoizat/template-haskell-project.svg?label=version&style=flat-square)](build.gradle)<br/>
[![Waffle.io - Columns and their card count](https://badge.waffle.io/pascalpoizat/template-haskell-project.svg?columns=all)](https://waffle.io/pascalpoizat/template-haskell-project)

<!--
[![Version](https://img.shields.io/hackage/v/template-haskell-project.svg?label=version&amp;style=flat-square)](https://hackage.haskell.org/package/template-haskell-project)
-->

This is a simple template for Haskell projects built with Stack.

## Installation

There are two ways to use the template.

### 1. Cloning the template and then setting up the stack project

First clone the template:

```
git clone https://github.com/pascalpoizat/template-haskell-project.git
```

Rename the project, go into it, and rename the `.cabal` file.

```
mv template-haskell-project projectname
cd projectname
mv template-haskell-project.cabal projectname.cabal
```

Edit the `projectname.cabal` file to replace references to `template-haskell-project` by `projectname`
(there should be 7 instances). 
Do the same for the `.travis.yml` file (there should be 3 instances),
the `README.md` file (there should be 9 instances), and
the `run` file (there should be 1 instance). If you have the `sed` command on your system you can do it easily using:

```
sed -i.old 's/template-haskell-project/projectname/g' projectname.cabal
sed -i.old 's/template-haskell-project/projectname/g' .travis.yml
sed -i.old 's/template-haskell-project/projectname/g' README.md
sed -i.old 's/template-haskell-project/projectname/g' run
```

with `.old` backup files being created in case something goes wrong.

To test if all is ok you can use one or several of these commands:

```
stack build
stack test
stack exec projectname-exe
./run
```

To end, you will certainly remove the reference to the template remote repository using:

```
git remote rm origin
```

and then use you own repository.

### 2. Create a new local stack project and then getting code from the template

The first step to create your project is:

```
stack new projectname
```

Then go into the new directory that has been created and initialize `git` versioning.

```
cd projectname
git init
```

Now you can setup the remote for getting the template and get it.

```
git remote add origin https://github.com/pascalpoizat/template-haskell-project.git
git fetch
git checkout -ft origin/master
```

You no longer need the remote (of course you may then have your own one to work with).

```
git remote rm origin
```

Before playing around with the project you will have to change names in some places.
First rename `template-haskell-project.cabal` into `projectname.cabal`.

```
mv template-haskell-project.cabal projectname.cabal
```

Edit the `projectname.cabal` file to replace references to `template-haskell-project` by `projectname`
(there should be 7 instances). 
Do the same for the `.travis.yml` file (there should be 3 instances),
the `README.md` file (there should be 9 instances), and
the `run` file (there should be 1 instance). If you have the `sed` command on your system you can do it easily using:

```
sed -i.old 's/template-haskell-project/projectname/g' projectname.cabal
sed -i.old 's/template-haskell-project/projectname/g' .travis.yml
sed -i.old 's/template-haskell-project/projectname/g' README.md
sed -i.old 's/template-haskell-project/projectname/g' run
```

with `.old` backup files being created in case something goes wrong.

To test if all is ok you can use one or several of these commands:

```
stack build
stack test
stack exec projectname-exe
./run
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
- [containers](https://hackage.haskell.org/package/containers) for various containers such as Set
- [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative) for parsing command line options
- [fgl](http://hackage.haskell.org/package/fgl) for (inductive) graph structures
- [aeson](https://hackage.haskell.org/package/aeson) for JSON related tasks
- [HaXml](https://hackage.haskell.org/package/HaXml) for XML related tasks

### Testing

- [Tasty](http://documentup.com/feuerbach/tasty) a testing framework that enables one to combine different kinds of testing (typically, the ones below)
- [HUnit](https://github.com/hspec/HUnit#readme) for unit testing
- [SmallCheck](https://github.com/feuerbach/smallcheck#readme) for property based testing
- [QuickCheck](https://github.com/nick8325/quickcheck#readme) for random based testing
- [HSpec](http://hspec.github.io) BDD testing

### Development

- For VCS and commit messages, see [here](https://github.com/pascalpoizat/template-java-project/blob/master/README.md), "Source repository and VCS"
- [Stack](https://haskellstack.org/) build system
- [Cabal](https://www.haskell.org/cabal/) used by Stack
- [Travis CI](https://travis-ci.org) for continuous integration
- [Stack and Travis CI](https://docs.haskellstack.org/en/latest/travis_ci/) for continuous integration
- [HPC](https://wiki.haskell.org/Haskell_program_coverage) for code coverage
- [Coveralls](https://coveralls.io) for code coverage
- [stack-hpc-coveralls](https://github.com/rubik/stack-hpc-coveralls) for code coverage (does not seem to work with Stack lts-8.3)

### IDE

- [Haskell IDE Engine (HIE)](https://github.com/haskell/haskell-ide-engine) together with the [Haskell Language Server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) for [Visual Studio Code](https://code.visualstudio.com) (note: HIE can also be used with other editors, see the doc)
- [Haskero](https://gitlab.com/vannnns/haskero/blob/master/README.md) for [Visual Studio Code](https://code.visualstudio.com)
- [Haskell layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell) for [Spacemacs](http://spacemacs.org)

see also more generally this very nice project: [haskell-ide-chart](https://github.com/rainbyte/haskell-ide-chart).

### TODO

see: [issues](https://github.com/pascalpoizat/template-haskell-project/issues)

## Acknowledgements

I would like to thank these people (or the people behind these accounts) for help with setting up a working Haskell environment and updating it from time to time.

[@AkiiZedd (twitter)](https://twitter.com/AkiiZedd),
[@alanz (github)](https://github.com/alanz),
[@BeRewt (twitter)](https://twitter.com/BeRewt),
[@kaddourkardio (twitter)](https://twitter.com/kaddourkardio), 
[@SergeStinckwich (twitter)](https://twitter.com/SergeStinckwich),
[@spacemacs (twitter)](https://twitter.com/spacemacs),
[@rainbyte (github)](https://github.com/rainbyte)
