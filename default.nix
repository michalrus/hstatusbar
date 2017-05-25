{ nixpkgs ?
    # Default for CI reproducibility, optionally override in your configuration.nix.
    (import ((import <nixpkgs> {}).pkgs.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs";
      rev = "22da5d02466ffe465735986d705675982f3646a0";
      sha256 = "1nmxiv72z832dhz6n8g0c0gsahvkvb27f7m6gxmcld03lh1v0kmb";
    }) {})
, haskellPackages ? nixpkgs.haskellPackages
}:

with nixpkgs;

let

  build = let # TODO: Consider using `cabal sdist`? https://git.io/vSo8l
    src = builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
         lib.all (i: toString i !=            path) [ ./.git ./dist ./result ]
      && lib.all (i:          i != baseNameOf path) [ ".stack-work" ])
      ./.;
  in haskellPackages.callCabal2nix "hstatusbar" src {};

  env = lib.overrideDerivation build.env (oldAttrs: {
    buildInputs = [ git ] ++ (with haskellPackages; [
      cabal-install hlint hindent stylish-haskell
    ]);
  });

in build // { inherit env; }
