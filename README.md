# template-haskell-project

A simple template for Haskell projects built with Stack 

Tested with Stack 2.9.1 and resolver lts-20.5 / GHC 9.2.5.

**new: support for multiple builds and binaries deployment**

*soon (maybe): updated information on how to install the Haskell tool suite (ghcup, GHC, cabal, stack, language server, ...), coverage information using coveralls.io*

[![Build Status](https://img.shields.io/travis/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://app.travis-ci.com/github/pascalpoizat/template-haskell-project)
[![License](https://img.shields.io/github/license/pascalpoizat/template-haskell-project.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/github/tag/pascalpoizat/template-haskell-project.svg?label=version&style=flat-square)](template-haskell-project.cabal)<br/>
<!--
[![Code Coverage](https://img.shields.io/coveralls/pascalpoizat/template-haskell-project/master.svg?style=flat-square)](https://coveralls.io/github/pascalpoizat/template-haskell-project)
[![Waffle.io - Columns and their card count](https://badge.waffle.io/pascalpoizat/template-haskell-project.svg?columns=all)](https://waffle.io/pascalpoizat/template-haskell-project)
-->
<!--
[![Version](https://img.shields.io/hackage/v/template-haskell-project.svg?label=version&amp;style=flat-square)](https://hackage.haskell.org/package/template-haskell-project)
-->

## Create the project

The first step to create your project is:

```
stack new projectname --resolver lts-20
```

To test if all is ok, run:

```
stack clean && stack build && stack test && stack exec projectname-exe
```

The result should be:

```sh
[... lots of text ...]
someFunc
```

Create a new repository on GitHub, named `projectname`.
Then go into the new directory that has been created by `stack new` and initialize `git` versioning.

```sh
cd projectname
git init
git remote add origin https://github.com/youraccount/projectname.git
git add * .*
git commit -m "initial commit"
git push -u origin master
```

At this point you may consider replacing the default `LICENSE` by one of your choice (choosing one from GitHub will make it recognize it and the license badge in the readme will be automatic) and edit the `README.md` file (e.g., by reusing this one).

We will use branch `master` for final releases (and version tags) and a second branch, `develop` for snapshot releases.
You may of course have also feature-specific branches but remind that version tags should only be pushed to `master`.

```sh
git checkout -b develop
git push origin develop
git checkout master
```

## Adding continuous integration to the picture

** THIS PART SHOULD BE UPDATED WITH THE MOVE TO travis-ci.com**

We will use [Travis CI](https://travis-ci.org).

Our objective is:

- have the CI build your application with `stack` for 2 environments
	- LTS-20.5, GHC 9.2.5, linux, x86_64
	- LTS-20.5, GHC 9.2.5, osx, x86_64
- have the CI deploy binaries to GitHub releases
	- a final release for tagged commits (in our scenario, make tags only on branch `master`)
	- a draft release for commits on branch `develop`)

First, go at [https://travis-ci.org](https://travis-ci.org), connect your GitHub account to it and make sure that your `projectname` is activated [there](https://travis-ci.org/account/repositories). If it does not appear, sync your account (upper left).

Create a file named `.travis.yml` at the root of your project with [these contents](https://github.com/pascalpoizat/template-haskell-project/blob/master/.travis.yml), and replacing any occurence of `template-haskell-project` by `projectname`.

Save the `deploy` part of it somewhere (a file or your clipboard). You will need it when it is erased in the next step.

To get a secure token for Travis CI (with a read access on all your public repos) install the Travis CI client (see [here](https://github.com/travis-ci/travis.rb#installation)) and run:

```sh
travis setup releases --force
Username: (enter your user name at GitHub)
Password for username: (enter your password at GitHub)
File to Upload: (type enter)
Deploy only from username/projectname? |yes| (type enter)
Encrypt API key? |yes| (type enter)
```

This will change this in your `.travis.yml`:

```yaml
deploy:
  provider: releases
  api_key:
    secure: (KEEP THIS)
  file: ''
  on:
    repo: username/projectname
```

Replace this new `deploy` part by the one you have saved before but replacing the secure information by the one that has been generated for you.

Travis will use your application itself as a way to get its version. So change the file `src/Lib.hs' as follows.

Before:

```haskell
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

After:

```haskell
someFunc :: IO ()
someFunc = putStrLn "0.1.0.0"
```

You can also change directly `app/Main.hs`. What is important is that your application, when run, returns its version (later on you may want to have a command-line option to get the option as what I did [here](https://github.com/pascalpoizat/fbpmn)) and, accordingly, change the command used in `.travis.yml` file to retrieve the version.

**Important:** whenever you change the version you will have to keep 3 things consistent:

- the version in file `package.yaml` (`projectname.cabal` being then generated from it) or in file `projectname.cabal` (if there is no `package.yaml` file).
- the version your application returns when being called.
- the git tag that you push to GitHub.

Now it is time to trigger a Travis build.

```sh
git add * .*
git commit -m "initial CI commit"
git push origin master 
```

If you go to your [Travis dashboard](https://travis-ci.org/dashboard) you should see your project building and then passing the build. But if you go to the GitHub release section of your repository, nothing appears.

This is normal. To generate a release wrt. `master`, we have to add a tag on it.

```sh
git tag -a v0.1.0.0 -m "v0.1.0.0"
git push origin --tags
```

Travis runs again but then deploys the binaries under the GitHub release section of your repository. You may comment this release and publish it for your visitors to see.

Now let us move to branch `develop`, retrieve things from `master` and push to the repository.

```sh
git checkout develop
git merge master
git push origin develop
```

If you go to you the GitHub release section of your repository you can see that Travis has made a draft release. This release is visible only from your collaborators (you may of course publish it).

To conclude:

- each time you have a new stable version, create a version tag on `master` and Travis will create a final release.
- each time you commit to `develop`, Travis will overwrite the SNAPSHOT draft release for for the current version at `develop` (usually the next one after the one at `master`).
- create version tags only from `master`.

## Haskell notes

These are some interesting references related either to learning Haskell or that have been used to set up this template.

### Documentation

- [Learn you a Haskell for great good!](http://learnyouahaskell.com) #1 to read 
- [Haskell Programming from first principles (Haskell Book)](http://haskellbook.com) #2 to read
- [Haskell Wiki](https://wiki.haskell.org/FAQ)
- [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)

### Packages

Archives:

- [Hackage](https://hackage.haskell.org) to look for nice packages to use in your applications.
- [Stackage](https://www.stackage.org) to find which packages are in the LTS releases you will use with `stack`.

Some packages:

- [containers](https://hackage.haskell.org/package/containers) for various containers such as Set
- [optparse-applicative](http://hackage.haskell.org/package/optparse-applicative) for parsing command line options
- [aeson](https://hackage.haskell.org/package/aeson) for JSON related tasks
- [xml](https://hackage.haskell.org/package/xml) for simple XML related tasks or [HaXml](https://hackage.haskell.org/package/HaXml) for more complicated XML related tasks

### Testing

- [Tasty](http://documentup.com/feuerbach/tasty) a testing framework that combines different kinds of testing (typically, the ones below)
- [HUnit](https://github.com/hspec/HUnit#readme) for unit testing
- [SmallCheck](https://github.com/feuerbach/smallcheck#readme) for property based testing
- [QuickCheck](https://github.com/nick8325/quickcheck#readme) for random based testing
- [HSpec](http://hspec.github.io) BDD testing

### Development

- For VCS and commit messages, see [here](https://github.com/pascalpoizat/template-java-project/blob/master/README.md), "Source repository and VCS"
- [Stack](https://haskellstack.org/) build system
- [Cabal](https://www.haskell.org/cabal/) used by Stack
- [Travis CI](https://travis-ci.org) for continuous integration
- [Stack and Travis CI](https://docs.haskellstack.org/en/stable/travis_ci/) for continuous integration
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
