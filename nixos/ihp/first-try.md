
## Debug

When something goes wrong you can also run `haskell-language-server` inside the project directory (within a `nix-shell`). This might output some helpful error messages.

error: file 'nixpkgs' was not found in the Nix search path (add it using $NIX_PATH or -I)

       at /nix/store/1picgyzmhvjkrxfwpwqs9h9ka7mb8rdj-source/NixSupport/make-nixpkgs-from-options.nix:77:27:

           76|
           77|   pkgs = (import ((import <nixpkgs> {}).fetchFromGitHub {
             |                           ^
           78|     owner = "NixOS";
(use '--show-trace' to show detailed location information)


