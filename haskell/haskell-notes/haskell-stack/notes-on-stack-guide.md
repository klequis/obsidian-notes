- What is Stackage


- 1 stop shop vs ??
- "curated package sets called snapshots."
  - where are snapshots
- stack purge
  - "deletes local stack working directories" - what/where are they
  - "including extra-deps" - what/where are they
  - "snapshot packages, compliers or programs installed using `stack install`" - same question

- "such as building test and Haddocks at"
  - what are Haddocks


- A [list of all modules](https://www.stackage.org/lts/docs) in a snapshot
  - very slow link
  - same as previous link in the doc
  
- "curated package sets" used in multiple places in the doc and seems to include snapshots, other stuff that goes in "extra-deps" and I am left wondering what else.

- "LTS resolver" is a term I'm still not comfortable with.

- Making a list of all links in the doc, what they are and how they relate would be helpful.

- "please see the resolve from the link above to get the latest." 
  - doesn't `stack new` always use the latest LTS?
  - oops, it appears to but he is suggesting switching to the latest nightly
  - so the answer here is that you go to the page for the nightly LTS and copy the resolver name from the top of the doc, directly under the published date which currently reads 
    - "resolver: nightly-2022-05-25"
- related to the previous point
  - when stack.yaml is created by `stack new` the *resolver* field contains a `url:` that points to "https://raw.githubusercontent.com/..."
  - but it appears that you can also use

## `stack --resolver
- `stack --resolver lts-18.3 build` doesn't change the resolver field in stack.yaml and once don't the resolver field seems to be ignored.
- then changing the resolver field does cause a new build
- **I think I should just ignore this**.
---

You can view a list of packages in a snapshot
- https://www.stackage.org/lts-19.8
As well as all the modules in a snapshot
- https://www.stackage.org/lts-19.8/docs


## Config file fields

### package.yaml
"dependencies"
- a field in package.yaml
- list here packages you use that are part of the snapshot referenced in the "resolver" filed listed in stack.yaml


### stack.yaml
"resolver"
- has a url that points to a snapshot
- e.g., https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/8.yaml


## "snapshots"
- are just a list of packages in a yaml file that are compatible
- **really, I don't understand all that is in those files**
- the repo for snapshots is: https://github.com/commercialhaskell/lts-haskell

### Where are snapshots kept?
- there is a folder ~/.stack/snapshots: is that the location?
- If I delete all the folders in ~/.stack/snapshots and then rebuild my package a new one is placed there.
- the filder name is "437eca8097bac2abeca01f461e61d69d901ea5abaff2c4b4af026108f0ab75f9"
- it has a 9.0.2 sub folder which is the same GHC version I get using `stack exec -- which ghc` in project root.
- **connection between snapshot and project**
  - project-root/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.1.0/stack-build-caches/437eca8097bac2abeca01f461e61d69d901ea5abaff2c4b4af026108f0ab75f9.
  - the guid above matches the snapshot in ~/.stack/snapshots/x86_64-linux-tinfo6
  - `stack purge` does not remove the snapshot. It does completely remove .stack-work
  
  
  

  


