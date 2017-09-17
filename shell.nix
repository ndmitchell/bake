{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cmdargs, containers
      , deepseq, direct-sqlite, directory, disk-free-space, extra
      , filepath, hashable, HTTP, http-client, http-conduit, http-types
      , old-locale, process, random, safe, shake, smtp-mail
      , sqlite-simple, stdenv, text, time, transformers
      , unordered-containers, wai, wai-extra, warp
      }:
      mkDerivation {
        pname = "bake";
        version = "0.5";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base bytestring cmdargs containers deepseq direct-sqlite
          directory disk-free-space extra filepath hashable HTTP http-client
          http-conduit http-types old-locale random safe shake smtp-mail
          sqlite-simple text time transformers unordered-containers wai
          wai-extra warp
        ];
        executableHaskellDepends = [
          aeson base bytestring cmdargs containers deepseq direct-sqlite
          directory disk-free-space extra filepath hashable HTTP http-client
          http-conduit http-types old-locale process random safe shake
          smtp-mail sqlite-simple text time transformers unordered-containers
          wai wai-extra warp
        ];
        homepage = "https://github.com/ndmitchell/bake#readme";
        description = "Continuous integration system";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
