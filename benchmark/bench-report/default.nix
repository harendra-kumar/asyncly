{
  nixpkgs ?
    # nixpkgs 21.05 needs a revised version of bench-show and we do not
    # know how to use a revised version in nix.
    #import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz)
    import (builtins.fetchTarball https://github.com/composewell/nixpkgs/archive/01dd2b4e738.tar.gz)
        {}
#, compiler ? "ghc884" # For nix 21.05
, compiler ? "default"
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    bench-report = mkPackage super "bench-report" ./. "" inShell;
                };
        };

    drv = mkHaskellPackages true;

    shell = drv.shellFor {
        packages = p:
          [ p.bench-report
          ];
        # Use a better prompt
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).bench-report
