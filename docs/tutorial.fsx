(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../src/FSharp.Collections.ParallelSeq/bin/Debug/netstandard2.0"

(**
Tutorial
========================

Here is an example of using the parallel sequence combinators:
*)
#r "FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq

let isPrime n = 
    let upperBound = int (sqrt (float n))
    let hasDivisor =     
        [2..upperBound]
        |> List.exists (fun i -> n % i = 0)
    not hasDivisor
        
let nums = [|1..500000|]
let finalDigitOfPrimes = 
        nums 
        |> PSeq.filter isPrime
        |> PSeq.groupBy (fun i -> i % 10)
        |> PSeq.map (fun (k, vs) -> (k, Seq.length vs))
        |> PSeq.toArray  

let averageOfFinalDigit = 
    nums 
    |> PSeq.filter isPrime
    |> PSeq.groupBy (fun i -> i % 10)
    |> PSeq.map (fun (k, vs) -> (k, Seq.length vs))
    |> PSeq.averageBy (fun (k,n) -> float n)

let sumOfLastDigitsOfPrimes = 
    nums 
    |> PSeq.filter isPrime
    |> PSeq.sumBy (fun x -> x % 10)

