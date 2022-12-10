let
  revision = "813836d64";

  tarball = owner: repo: rev:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    };

  compilerSet = pkgs.haskell.packages."ghc902";

  pkgs = import (tarball "NixOS" "nixpkgs" revision) { inherit config; };
  ignore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          okasaki = super.callCabal2nix "okasaki"
                      (ignore [./.gitignore] ./.) {};
        };
      };
    };
  };

in
  {
    inherit pkgs;
    shell = compilerSet.shellFor {
      packages = p: [p.okasaki];
      buildInputs = with pkgs; [
        compilerSet.cabal-install
      ];
    };
  }
