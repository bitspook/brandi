let
  sources = import ./nix/sources.nix;
in
{ compiler ? "ghc883"
, pkgs ? import sources.nixpkgs { }
}:

let
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      hakyll = hpOld.hakyll.overrideAttrs(oldAttrs: {
        configureFlags = "-f watchServer -f previewServer";
        patches = [ ./hakyll.patch ];
      });

      brandi = hpNew.callCabal2nix "brandi" ./. { };

      niv = import sources.niv { };
    };
  };

  project = pkgs.haskell.lib.justStaticExecutables haskellPackages.brandi;

in
{
  project = project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      brandi
    ];
    buildInputs = with haskellPackages; [
      brandi
    ];
    withHoogle = true;
  };
}
