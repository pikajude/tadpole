{ nixpkgs ? (import <nixpkgs> {}) }:

let
  reflex-unpatched = pkgs.fetchgit {
    url = "https://github.com/reflex-frp/reflex-platform";
    rev = "cb37f01ff479306b60544a6af6f6f6e7cf3746e6";
    fetchSubmodules = true;
    sha256 = "1gkpifiywi1adad9d6v7a178zhs8slq6gq2sqryqnisc93glvww2";
  };

  reflex = pkgs.runCommand "reflex" {} ''
    mkdir -p $out
    cp -R ${reflex-unpatched}/* $out
    sed -i '/doHaddock/d' "$out/default.nix"
  '';

  inherit (nixpkgs) pkgs;

  reflex-platform = pkgs.callPackage reflex {
    nixpkgsFunc = import nixpkgs.pkgs.path;
    # inherit config;
  };

  inherit (pkgs) lib;
  inherit (lib) inNixShell;

  pkg = with pkgs.haskell.lib; (reflex-platform.ghc.callPackage ./. {}).overrideScope (self: super: {
    reflex-gloss = doJailbreak super.reflex-gloss;
  });
in if inNixShell then pkg.env else pkg
