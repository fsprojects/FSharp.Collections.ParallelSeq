(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
F# Parallel Sequences
===================

This library provides an F#-style API for the parallel operations on sequences from the
System.Linq.ParallelEnumerable class in .NET 4.0. The API is akin to F# operations on sequences.


<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The F# FSharp.Collections.ParallelSeq library can be <a href="https://nuget.org/packages/FSharp.Collections.ParallelSeq">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Collections.ParallelSeq</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Example
-------

This example demonstrates the use of a function defined in this library.

*)
#r "FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq

let nums = [|1..500000|]
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
