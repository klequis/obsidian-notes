**Useful Commands, etc.**
- `haskell-language-server --probe-tools`
- hls process name "haskell-language-server"

**Install Prerequisites**
- build-essential 
- curl 
- libffi-dev - add
- libffi8ubuntu1 - not found
- libgmp-dev  - add
- libgmp10 - already installed
- libncurses-dev - add
- libncurses5 - add
- libtinfo5 - already marked

May also need
  - libicu-dev - not installed so installed it
  - libncurses-dev - not installed so installed it
  - libgmp-dev - already installed
  - zlib1g-dev - already installed

## 1. build-essential packages
![[Pasted image 20220715091130.png]]
## 2. Install curl
- done

## 3. Install other prerequisites

![[Pasted image 20220715091811.png]]

## 4. Install GHC, cabal-install and haskell-language-server via [GHCup](https://www.haskell.org/ghcup/)

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## 5. To install Stack, follow the [Stack installation guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Result:
```console
[ Info  ] verifying digest of: stack-2.7.5-linux-x86_64-static.tar.gz
[ Info  ] Unpacking: stack-2.7.5-linux-x86_64-static.tar.gz to /tmp/ghcup-73526ed987bc8ca3
[ Info  ] Installing stack
[ Info  ] Stack installation successful
[ Info  ] Stack manages GHC versions internally by default. In order to make it use ghcup installed
[ ...   ] GHC versions you can run the following commands:
[ ...   ]   stack config set install-ghc false --global
[ ...   ]   stack config set system-ghc  true  --global
[ ...   ] 
[ ...   ] On windows, you may find the following config options useful too:
[ ...   ]   skip-msys, extra-path, extra-include-dirs, extra-lib-dirs
[ ...   ] 
[ ...   ] Also check out: https://docs.haskellstack.org/en/stable/yaml_configuration
[ ...   ] 
[ ...   ] !!! Additionally, you should upgrade stack only through ghcup and not use 'stack upgrade' !!!
[ ...   ] 
```


This part is of interest but not doing what it suggests now
[ Info  ] Stack manages GHC versions internally by default. In order to make it use ghcup installed
[ ...   ] GHC versions you can run the following commands:
[ ...   ]   stack config set install-ghc false --global
[ ...   ]   stack config set system-ghc  true  --global

## 6. Install fonts-firacode

## 7. Set GHC version with ghcup tui

When I first launched VS Code got message about stack needing to download GHC v9.0.2. Instead of doing that I used `ghcup tui` to set the current version to v9.0.2.

To avoid this, set to v9.0.2 before installing VSC extensions

## 8. VS Code Extensions

- Haskell: Haskell language supported powered by ..., Haskell
- Haskell Syntax Highlighting, JustusAdam

> Hopefully that is all.

****

**NEW**
- Check HLS same v as GHC
- generally can use haskell-language-server-wrapper for haskel-language-server.
- per the haskell-language-server documentation, the following need to be installed
  - libicu-dev - not installed so installed it
  - libncurses-dev - not installed so installed it
  - libgmp-dev - already installed
  - zlib1g-dev - already installed
  - *did not fix the problem :(*
  - restart computer
  - *no change*
- When I launch VSC I see "haskell-language-server-wrapper" running but properties does not show its location and it soon exits.
- Made video of process when launching VSC
  - first haskell-language-server-wrapper launches
  - then 2 instances of haskell-language-server-9.0.2 appear
  - the 9.0.2 instances go away
  - the come back and quickly dissapear.
  - shortly thereafter haskell-language-server-wrapper exits.
  - Check GHC version
    - version is 8.10.7 - does not match HLS version!!
  - Used ghcup tui to change ghc v to 9.0.2 to match language server *did not help*.

**NEW - change some settings**

- Ghcup Executable Path: /home/klequis/.ghcup/bin/ghcup
- 

**More steps**

- in stack.yaml uncomment `system-ghc: true`
- WORKING - finally the above fixed the problem. Doesn't seem like a good fix gut fine for now.

## Settings of interest

- Ghcup Executable Path
  - Manually set a ghcup executable path
- Log File
  - redirects log to a file
- Manage HLS
  - PATH, GHCup


---

**Haskell: Ghcup Executable Path**
- Current: full path
- Should it be the full path?

**Haskell: Manage HLS**
- current: PATH
  - try: GHCup - NO
- other options
  - GHCup
  - Discover in system path

**Haskell: Server Executable Path**
??

**Haskell › Hlint: Executable Path**
current: /home/klequis/.vscode/extensions/hoovercj.haskell-linter-0.0.6/out/src/features/hlintProvider.js


## Errors
### No Intellisense in Stack Project
- Checked and found no errors in VSCode > Output
- Restartin Haskell Language Server (ctrl+shift+p) did not help
- Restarted computer and then problem was solved


### ABIs don't match!

> No useful information here. Maybe delete
```json

{
"window.zoomLevel": 2,
"editor.minimap.enabled": false,
"editor.fontFamily": "Fira Code",
"editor.fontLigatures": true,
"haskell.hlint.executablePath": "/home/klequis/.vscode/extensions/hoovercj.haskell-linter-0.0.6/out/src/features/hlintProvider.js",
"security.workspace.trust.untrustedFiles": "open",
"haskell.ghcupExecutablePath": "/home/klequis/.ghcup/bin/ghcup",
"haskell.hlint.hints": [
]
}
```


## Instructions

**stack.yaml**

```yaml
setup-info-locations:
- setup-info.yaml
```

**setup-info.yaml**
```yaml
ghc:
  linux64-tinfo6:
    9.0.2:
      url: "https://downloads.haskell.org/ghc/9.0.2/ghc-9.0.2a-x86_64-fedora27-linux.tar.xz"
```


## Cradle
The cradle is essentially a configuration that maps file paths with "components" (libs, executables, test suites, etc).


## Pending Investigation
- If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!
- 2022-05-24T21:49:27.568738Z | Warning | Currently, HLS supports GHC 9 only partially. See [issue #297](https://github.com/haskell/haskell-language-server/issues/297) for more detail.

- ghcide compiled by GHC 9.0 failed to load packages: <command line>: cannot satisfy -package vscode-haskell-config-0.1.0.0  
  (use -v for more information).  
  Please ensure that ghcide is compiled with the same GHC installation as the project.cradle

## Errors and Resolution

### Multi Cradle: No prefixes matched

Module is not listed in the build config files (.cabal or package.yaml).
- You can use an explicit configuration, creating a `hie.yaml` file in the project root listing the prefixes and try to add devel.hs to load it in the IDE: see [https://github.com/haskell/haskell-language-server#configuring-your-project-build](https://github.com/haskell/haskell-language-server#configuring-your-project-build) for mor info


### The Haskell (...) server crashed 5 times in the last 3 minutes. The server will not be restarted.

- In stack.yaml uncomment `system-ghc: true`


## Log
- Haskell addin `haskell.haskell`
  - last fully supported ghc version 8.10.7
  - check - i am using 8.10.7
  - switch to 9.x
    - no difference :(
  - Restore 8.10.7
- Install hlint
  - Looked for existing install of hlint. Didn't find on disk or via `cabal list --installed`
  - `> cabel install hlint`
- set `"haskell.hlint.executablePath"` to `"hlint"`
  - per suggestion: https://github.com/hoovercj/vscode-haskell-linter/issues/38#issuecomment-1116515347
- Setting: Haskell > Hlint: Executable Path
  - is: /home/klequis/.vscode/extensions/hoovercj.haskell-linter-0.0.6/out/src/features/hlintProvider.js
  - change to: ""
  - result: "Cannot hlint the haskell file. The hlint program was not found. Use the 'haskell.hlint.executablePath' setting to configure the location of 'hlint'"
  - change back

- `{-# LANGUAGE OverloadedStrings #-}` is no longer flagged as error and is being auto-added when I run `stack build simpleCalc`
- I am no longer getting the "ABIs don't match" error.

## Links
- Issue on path problems: https://stackoverflow.com/questions/43983718/set-global-path-environment-variable-in-vs-code
- [Haskell Language Server troubleshooting](https://haskell-language-server.readthedocs.io/en/latest/troubleshooting.html)
- [Haskell Language Server issue tracking](https://github.com/haskell/haskell-language-server/issues)
- 