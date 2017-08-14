let
  compiler                = "ghc821";

  hostPkgs                = import <nixpkgs> {};
  id                      = x: x;
  fetchNixpkgsWithNixpkgs = nixpkgs: nixpkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./srcs/nixpkgs.json));
  fetchNixPkgs            = fetchNixpkgsWithNixpkgs hostPkgs;
  nixpkgs                 = fetchNixPkgs;
  pkgs                    = import nixpkgs {};
  ghcOrig                 = pkgs.haskell.packages.${compiler};

  localSrc                =      repo: rev: sha256:    pkgs.fetchgit { url = "file:///home/deepfire/src/" + repo; rev = rev; sha256 = sha256; };
  githubSrc               =      repo: rev: sha256:    pkgs.fetchgit { url = "https://github.com/"        + repo; rev = rev; sha256 = sha256; };
  overC                   =                            pkgs.haskell.lib.overrideCabal;
  overGithub              = old: repo: rev: sha256: f: overC old (old: f (old // { src = githubSrc repo rev sha256; }));
  overHackage             = old: version:   sha256: f: overC old (old: f (old // { version = version; sha256 = sha256; }));
  overLocal               = old: repo: rev: sha256: f: overC old (old: f (old // { src = localSrc repo rev sha256; }));

  ghc = ghcOrig.override {
    overrides = self: super: with pkgs.haskell.lib; {
      mkDerivation = drv: super.mkDerivation (drv // { doCheck = false; });
      # primitive = doJailbreak super.primitive;
      # syb       = doJailbreak super.syb;
      # vector    = doJailbreak super.vector;
      libmpd     = doJailbreak (overGithub super.libmpd
                    "vimus/libmpd-haskell"  "ce4e441b5d194ea868ddfccfc37a0344e2b97e47" "02xkwrnf4kgyhhz2vcxyvx5aikl72ym51qf5h4rf4ds84lcgak1x" id);
      list-t     = doJailbreak (overGithub super.list-t
                    "nikita-volkov/list-t"  "db3354e352fd0fa92f1cef8f1fc05e6fd0047e63" "0h9l7zg4s3fkl9rcd2cdhgcqgizff4zc6rzpdsl1kyqa3d79kd17" id);
      th-desugar = dontCheck (doJailbreak (overGithub super.th-desugar
                    "goldfirere/th-desugar" "7649ccb1509da95f2086b5048bc17bda55473b49" "1062qq85cdf9vb35fls7w55lvwdjqdjanj1ha24z75j3ifa69d0r" id));
      quickcheck-instances = doJailbreak (overGithub super.quickcheck-instances
                   "phadej/qc-instances"    "45ed46d8cba15235f786230f5e6876a2ba2604a8" "1br1b0chcr5ydkpi7njq85h6wyrxv707qm9xhr36wqwvdiz7zm08" id);
      singletons = doJailbreak (overGithub super.singletons
                   "goldfirere/singletons"  "d0fdb2cf02f29d6d076354696aaceb57f2715c85" "106iw4dsrgk6zsf49kbsiy3dg5q193bxihh3azxgf8gy48ymagck" id);
      zippers    = overGithub super.zippers
                   "deepfire/zippers"       "3724cde7ba89767b18a751d8c074e470370ecf87" "0rhmjyqjzni6725pq5vhpwlav6n2nbqjj9l3m7c7ba03yzvi2c8c" id;
    };
  };

  tools   = [ pkgs.cabal-install ];
  extras  = [  ];
  default = import ./default.nix;
  drv     = ghc.callPackage default {};
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              # libraryHaskellDepends    = old.libraryHaskellDepends    ++ tools ++ extras;
              executableHaskellDepends = old.executableHaskellDepends ++ tools ++ extras;
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
