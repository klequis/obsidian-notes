# Title: The haskell Tool Stack > User guide
---
#haskell-stack

source: [User guide](https://docs.haskellstack.org/en/stable/GUIDE)


## Create and Run a New Project
1. `stack new <project-name>`
2. `cd <project-name>`
3. `stack build`
4. `stack exec <project-name>-exe`



> These notes do not cover the complete guide. The last section covered is "Resolvers and changing your compiler version"


> List of all links is at bottom

## Definitions

- **Haddock**: Haskell API-level docs

## stack commands

- `stack new <project-name>`
- `stack build`
- `stack build` will run `stack setup` so no need to run the latter.
- Stack is built on top of Cabal
- `stack exec <project-name>-exe`
- `stack exec -- which ghc`
  - Finding the ghc version being used by Stack
- `stack clean` deletes directories in `.stack-work/dist` for all `.stack-work` directories in the project.
- `stack purge` deletes the local stack working directories, including extra-deps, git dependencies and the compiler output (including logs).
  - Returns project to state as if it were never built.
- `stack ls dependencies`  shows list of project's dependencies with version.
- `stack --resolver lts-18.3 build` will set `<project-root>/.`

stack is isolated. stack-built files generally go in either
- ~/.stack, or
- ./stack-work in the root of a project

snapshots and any stack-installed version of GHC to in ~/.stack

## Project files of interest

### projectName.cabal

Is updated automatically by `stack build` and should not be modified.

### stack.yaml
#### packages 
- List which project packages to build. 
- If only one in same directory: `'.'`.

#### resolver
- Points to a snapshot which can be found at: [Stackage>LTS](https://www.stackage.org/lts-19.8) (is pointing to current version)
- Can be a URL to a 
  - 'githubusercontent' page: e.g., https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/8.yaml
  -  or a resolver label from the snapshot page: "lts-19.8"

#### extra-deps
- List packages not in the snapshot here

#### flags
- No info yet

### Setup.hs
- Is a component of the Cabal build system. Technically not needed by stack.

## Adding dependencies

Add them to the `dependencies` section of `package.yaml`.

Add the `text` package.
```yaml
dependencies: 
- base >= 4.7 && < 5 
- text # added here
```

Then run `stack build`

After running `stack build` only `helloworld.cabal` we changed.

Add more packages

```yaml
dependencies:
- filepath
- containers
```

Again, only `helloworld.cabal` was changes.

## extra-deps

- Add `acme-missiles` to `dependencies` in `package.yaml`
- Run `stack build` -> error

**see next section for solution**

## Curated package sets

The book says: running `stack new` selected some **LTS resolver** for us.
- when we added the `text`  package it worked because `text` is part of the `LTS package set` that was selected.
- adding `acme-missiles` didn't work because it isn't part of the selected `LTS package set`.

### But what is "LTS resolver" and what is a "LTS package set"?

**LTS (Long Term Support) Haskell is a curated set of packages, a more stable companion to Stackage Nightly.**

> It turns out that `juhp` from the Los Gatos Reading Group works on this.

A **package set** is a list of packages in a `yaml` file. For example [lts-14.9.yaml](https://github.com/commercialhaskell/lts-haskell/blob/master/lts-14.9.yaml) *WARN: it is a big file!*


## Resolvers and changing your compiler version

> [YAML Configuration](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver) has additional and very important information.

You can pass a resolver flag to `stack build`, e.g., `stack --resolver lts-18.3 build`. I don't fully understand this as it is not directly connected ot the resolver field in stack.yaml. Ignoring it for now.



# Useful Links

- [Install/upgrade](https://docs.haskellstack.org/en/stable/install_and_upgrade/#installupgrade)stack
- [Cabal users guide](https://www.haskell.org/cabal/users-guide/developing-packages.html#developing-packages)
- [LTS Haskell 19.8 (ghc-9.0.2)](https://www.stackage.org/lts-19.8) Book had link to older version
- [hpack](https://github.com/sol/hpack)
- [LTS Resolver/LTS Haskell](https://github.com/commercialhaskell/lts-haskell#readme)
- LTS [# Snapshots](https://www.stackage.org/snapshots) List of all snapshots
- Stack [YAML Configuration](https://docs.haskellstack.org/en/stable/yaml_configuration)
- Stack [YAML Configuration > Resolver](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver)

