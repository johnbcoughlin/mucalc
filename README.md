mucalc
======

Haskell library for the Mu-Calculus

## Introduction
The *mu*-calculus is a logic for describing the behavior of discrete dynamical systems.
The [wiki page](http://en.wikipedia.org/wiki/Modal_%CE%BC-calculus) is a good overview.
Notably, it is possible to embed other temporal logics, including [Linear Temporal Logic](http://en.wikipedia.org/wiki/Linear_temporal_logic) within the *mu*-calculus.
This library includes facilities for specifying a **model** (as a labelled transition system), a set of **formulas** of the *mu*-calculus, and for computing the **realization** of formulas with respect to a given model.
