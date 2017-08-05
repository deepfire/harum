let
  compiler                = "ghc802";

  hostPkgs                = import <nixpkgs> {};
  fetchNixpkgsWithNixpkgs = nixpkgs: nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./srcs/nixpkgs.json));
  fetchNixPkgs            = fetchNixpkgsWithNixpkgs hostPkgs;
  nixpkgs                 = fetchNixPkgs;
  pkgs                    = import nixpkgs {};
  ghcOrig                 = pkgs.haskell.packages.${compiler};

  localSrc                =      repo: rev: sha256:       pkgs.fetchgit { url = "file:///home/deepfire/src/" + repo; rev = rev; sha256 = sha256; };
  githubSrc               =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/"        + repo; rev = rev; sha256 = sha256; };
  overC                   =                               pkgs.haskell.lib.overrideCabal;
  overCabal               = old:                    args: overC old (oldAttrs: (oldAttrs // args));
  overGithub              = old: repo: rev: sha256: args: overC old ({ src = githubSrc repo rev sha256; }    // args);
  overHackage             = old: version:   sha256: args: overC old ({ version = version; sha256 = sha256; } // args);
  overLocal               = old: repo: rev: sha256: args: overC old ({ src = localSrc repo rev sha256; }     // args);

  ghc = ghcOrig.override {
    overrides = self: super: with pkgs.haskell.lib; {
      # primitive = doJailbreak super.primitive;
      # syb       = doJailbreak super.syb;
      # vector    = doJailbreak super.vector;
    };
  };

  default = import ./default.nix;
  drv     = ghc.callPackage default {};
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                 [ pkgs.cabal-install pkgs.stack ghc.intero ];
             });
  drv''   = pkgs.lib.overrideDerivation
            drv'.env
            (old: {
              shellHook = ''
                export NIX_PATH=nixpkgs=${nixpkgs}
              '';
             });
in
  drv''
