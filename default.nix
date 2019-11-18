{ enableProfiling ? true, fast ? false }:
let
  bootstrap = import <nixpkgs> {};
  nixpkgs_json = builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs_json) rev sha256;
  };
  pkgs = import src {};

  compiler = "ghc865";
  ghc = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
      };
    };
  tools = with ghc; [ cabal-install ghcid ];
  flags = [ "--ghc-option=-Werror" ] ++ if fast
     then [ "--ghc-option=-O0" ] else [];

  overrideCabal = pkg: pkgs.haskell.lib.overrideCabal pkg
    ({buildDepends ? [], configureFlags ? [], ...}: {
      buildDepends = buildDepends ++ tools;
      enableLibraryProfiling = enableProfiling;
      configureFlags = configureFlags ++ flags;
    });
  cabal2nixResult = url: pkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix --no-check --jailbreak --no-haddock ${url} > $out
    '';
    buildInputs = [ pkgs.cabal2nix ];
  } "";
  cabal2nixResultLocal = path: cabal2nixResult "file://${path}";
  package = ghc.callPackage (cabal2nixResultLocal ./.) {};
  drv = overrideCabal package;

in if pkgs.lib.inNixShell then drv.env else drv

