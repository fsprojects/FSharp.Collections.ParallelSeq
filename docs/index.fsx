(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use
// it to define helpers that you do not want to show in the documentation.
#I "../src/FSharp.Collections.ParallelSeq/bin/Debug/netstandard2.0"

(**
F# Parallel Sequences
===================

This package provides an F#-style API for the parallel operations on sequences from the
System.Linq.ParallelEnumerable class in .NET. The API is akin to F# operations on sequences.


Example
-------

This example demonstrates the use of a function defined in this package.

*)
#r "FSharp.Collections.ParallelSeq.dll"

open FSharp.Collections.ParallelSeq

let nums = [| 1..500000 |]

let isPrime n =
    let upperBound = int (sqrt (float n))

    let hasDivisor =
        [ 2..upperBound ]
        |> List.exists (fun i -> n % i = 0)

    not hasDivisor

let finalDigitOfPrimes =
    nums
    |> PSeq.filter isPrime
    |> PSeq.groupBy (fun i -> i % 10)
    |> PSeq.map (fun (k, vs) -> (k, Seq.length vs))
    |> PSeq.toArray
(**
Samples & documentation
-----------------------

 * The [tutorial](tutorial.html) contains a further example.

 * The [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.

Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork
the project and submit pull requests. If you're adding new public API, please also
consider adding [samples][content] that can be turned into a documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under the Apache 2.0 license, which allows modification and
redistribution for both commercial and non-commercial purposes. For more information see the
[License file][license] in the GitHub repository.

  [content]: https://github.com/fsprojects/FSharp.Collections.ParallelSeq/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Collections.ParallelSeq
  [issues]: https://github.com/fsprojects/FSharp.Collections.ParallelSeq/issues
  [readme]: https://github.com/fsprojects/FSharp.Collections.ParallelSeq/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Collections.ParallelSeq/blob/master/LICENSE.txt
*)
