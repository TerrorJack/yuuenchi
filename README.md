# `yuuenchi`

An amusement park(monorepo) for kaijus(experimental Haskell modules). Once a kaiju(module) grows up, it may be sent(pasted) to other cities(projects) for destruction(production).

Contents:

* `DeBruijn`: Marshalling HOAS to De Bruijn indices.
* `FList`: A sequence type supporting O(1) `append`/`>>=`/`fromList`/`reverse`/`unfoldr`, meant as a `DList` replacement.
* `Freer`: Church-encoded freer monad.
* `GHCInception`: Get GHC info and run GHC API in a TH splice.
* `InlineEverything`: GHC source plugin which spares you the trouble of adding `INLINEABLE` annotations to every function/class method in a module.
* `LazyRead`: General abstraction of the input part of lazy I/O.
* `OneShotIO`: Ensure an `IO` action is run only once, latter invocations return cached result.
* `StrictByteStringBuilder`: Builder for strict `ByteString`s.
* `SyncExceptionIO`: Catch all sync exceptions, useful for working around lack of exception handling in modules like `Control.Monad.Par.IO`.
