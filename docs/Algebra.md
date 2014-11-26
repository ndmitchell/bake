# Algebra of Bake

This document outlines the algebra behind Bake, and it means for a patch to be accepted, and how to pick what should be done next.

## Types

    data Patch
    type State = [Patch]

The fundamental types in Bake are the `Patch` (think of a diff) and a `State` (the state of the repo). A `State` is just a sequence of `Patch` values, in order. As might be expected, we use `p` and `s` to denote values drawn from the sensible domains for each type.

    data Test
    tests :: Set Test

We define a `Test` (a single test case), and a set of `tests` which must pass. We assume values named `t` are drawn from `tests`. For the moment, we ignore the fact that different `State`s may induce different `Test`s, and that there may be dependencies between `Test`s.

    type History = Set (State, Test, Bool)

After running `Test`s, we accumulate a set of `State` (the state of the repo), `Test` (the test we ran) and `Bool` for the result of the `Test`. We assume `history` is a function that takes a triple and performs membership of a (changing) `History` value.

## Predicates

* A `State` is **blessed** if: _forall t, history (s, t, True)_
* A `Patch` is **blessed** if: _exists s, p in s && blessed s_
* A `Test` is **inconsistent** if: _exists s, history (s, t, True) && history (s, t, False)_
* A `Patch` is **plausible** if: _forall t, exists s, p in s && history (s, t, True)_
* A `Patch` is **faulty** if: _exists t, exists s, history (s, t, True) && history (s ++ [p], t, False)_. In addition, a passing test of `t` (on a different state) must be run after the failing test.

## Approach

We maintain a current `s` (assumed to be blessed), and a sequence of `ps` values. We want to prove every `Patch` to be either faulty or blessed.

If any state of `s` plus a prefix of `ps` becomes blessed, we roll that in as the new `State` and reduce `ps` to be only the non-included suffix.

We search for failures, that is a test that fails with state `s` plus a prefix of `ps`. We are keen to find failures that either make a patch blessed or plausible. Usually the focus will be on plausible, but at certain times of day (say during a 7pm freeze) the focus will become blessed until a blessed state has been reached.

Once we find a failure, we search for blame. The blame must lie either with a faulty patch, or an inconsistent test.

**Lemma:** In the absence of failures, we end up with everything blessed. Every failure results in a blame that excludes either a test or a patch. (I think the definition of faulty means this isn't quite true, but not sure exactly how.)

