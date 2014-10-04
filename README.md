# Bake [![Hackage version](https://img.shields.io/hackage/v/bake.svg?style=flat)](http://hackage.haskell.org/package/bake) [![Build Status](http://img.shields.io/travis/ndmitchell/bake.svg?style=flat)](https://travis-ci.org/ndmitchell/bake)

**Warning: This project is under heavy development and has never been used for real. You probably don't want to be the first person.**

Bake is a continuous integration server, designed for large, productive, semi-trusted teams. In particular it primarily targets:

* _Large teams_ where there are at least several contributors working on a single code base.
* _Productive teams_ which are regularly pushing code, many times a day.
* _Semi-trusted teams_ where code does not go through manual code review, but code does need to pass a test suite and perhaps some static analysis.

_Acknowledgements:_ The name "Bake" is thanks to Andy Adams-Moran
