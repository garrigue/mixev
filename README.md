# mixev
Mixin evaluators for the expression problem.

These evaluators are based on variant-reuse.pdf:
  Jacques Garrigue: Code reuse through polymorphic variants
  In <em>Workshop on Foundations of Software Engineering</em>.
  Sasaguri, Japan, November 2000.

* mixev.ml
  The evaluator presented in the paper.
* mixev2.ml
  Full source of the sum type version of the evaluator, requires -rectypes.
* mixin2.ml
  Larger example, using objects to structure code.
* mixmod.ml
  Original example, encoded using polymorphic variants and
  recursive modules (ocaml 3.07).
  Much more verbose, but does not use polymorphism.
* mixmod2.ml
  Same thing using normal sum types, without -rectypes.
  Does not work anymore with recent versions of OCaml.
* mixmod5.ml
  A tentative solution(?), using polymorphic variant, recursive
  modules, and private rows. Quite verbose, but each extension
  can be writeen with only O(n+m) boilerplate, where n is the number
  of types involved in the recursion, and m the number of operation.
* mixobj.ml
  A purely object-oriented version (without variants), using
  immediate objects (ocaml 3.08).
