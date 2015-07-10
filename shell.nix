{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cabal-install, cmdargs
      , containers, deepseq, direct-sqlite, directory, disk-free-space
      , extra, filepath, hashable, HTTP, http-types, old-locale, process
      , random, safe, shake, smtp-mail, sqlite-simple, stdenv, text, time
      , transformers, unordered-containers, wai, warp
      }:
      mkDerivation {
        pname = "bake";
        version = "0.3";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [
          aeson base bytestring cmdargs containers deepseq direct-sqlite
          directory disk-free-space extra filepath hashable HTTP http-types
          old-locale process random safe shake smtp-mail sqlite-simple text
          time transformers unordered-containers wai warp
        ];
        buildTools = [ cabal-install ];
        homepage = "https://github.com/ndmitchell/bake#readme";
        description = "Continuous integration system";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
