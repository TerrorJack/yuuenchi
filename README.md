# `yuuenchi`

An amusement park(monorepo) for kaijus(experimental Haskell modules). Once a kaiju(module) grows up, it may be sent(pasted) to other cities(projects) for destruction(production).

Contents:

* `DeBruijn`: Marshalling HOAS to De Bruijn indices.
* `FList`: A sequence type supporting O(1) append/flatMap/fromList/unfoldr, meant as a `DList` replacement.
* `GHCInception`: Get GHC info and run GHC API in a TH splice.
* `InlineEverything`: GHC source plugin which spares you the trouble of adding `INLINEABLE` annotations to every function/class method in a module.
