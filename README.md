# Bake [![Hackage version](https://img.shields.io/hackage/v/bake.svg?style=flat)](http://hackage.haskell.org/package/bake) [![Build Status](http://img.shields.io/travis/ndmitchell/bake.svg?style=flat)](https://travis-ci.org/ndmitchell/bake)

**Warning: This project is under heavy development and has never been used for real. You probably don't want to be the first.**

Bake is a continuous integration server, designed for large, productive, semi-trusted teams. In particular it primarily targets:

* _Large teams_ where there are at least several contributors working full-time on a single code base.
* _Productive teams_ which are regularly pushing code, many times a day.
* _Semi-trusted teams_ where code does not go through manual code review, but code does need to pass a test suite and perhaps some static analysis. People are assumed not to be malicious, but are fallible.

_Acknowledgements:_ The name "Bake" is thanks to Andy Adams-Moran.

## The Design

Bake is a Haskell library that can be used to put together a continuous integration server. To run Bake you start a single server for your project, which coordinates tasks, provides an HTTP API for submitting new patches, and a web-based GUI for viewing the progress of your patches. You also run some Bake clients which run the tests on behalf of the server. While Bake is written in Haskell, most of the tests are expected to just call some system command.

There are a few aspects that make Bake unique:

* Patches are submitted to Bake, but are not applied to the main repo until they have passed all their tests. There is no way for someone to "break the build" - at all points the repo will build on all platforms and all tests will pass.
* Bake scales up so that even if you have 5 hours of testing and 50 commits a day it will not require 250 hours of computation per day. In order for Bake to prove that a set of patches pass a test, it does not have to test each patch individually.
* Bake allows multiple clients to run tests, even if some tests are only able to be run on some clients, allowing both parallelisation and specialisation (testing both Windows and Linux, for example).
