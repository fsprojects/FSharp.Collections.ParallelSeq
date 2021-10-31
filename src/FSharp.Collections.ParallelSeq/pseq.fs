namespace FSharp.Collections.ParallelSeq

open System
open System.Linq
open System.Threading

// Type abbreviation for parallel sequences.
type pseq<'T> = ParallelQuery<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PSeq =

    // Converst a seq<'T> to a pseq<'T>.
    let inline toP (source: seq<'T>) =
        match source with
        | null -> nullArg "source"
        | :? pseq<'T> as p -> p
        | _ -> source.AsParallel()

    // Seq.* functions
    let empty<'T> = ParallelEnumerable.Empty<'T>()

    let length source = ParallelEnumerable.Count(toP source)

    let isEmpty source = not (ParallelEnumerable.Any(toP source))

    let singleton value = ParallelEnumerable.Repeat(value, 1)

    let head source = ParallelEnumerable.First(toP source)

    let truncate count source = ParallelEnumerable.Take(toP source, count)

    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) state source =
        ParallelEnumerable.Aggregate(toP source, state, Func<_, _, _>(folder))

    let reduce reduction source = ParallelEnumerable.Aggregate(toP source, Func<_, _, _>(reduction))

    let exists predicate source = ParallelEnumerable.Any(toP source, Func<_, _>(predicate))

    let forall predicate source = ParallelEnumerable.All(toP source, Func<_, _>(predicate))

    let exists2 (predicate: 'T -> 'U -> bool) source1 source2 =
        ParallelEnumerable.Any(
            ParallelEnumerable.Zip(toP (source1), toP (source2), Func<_, _, _>(predicate)),
            Func<_, _>(id)
        )

    let forall2 (predicate: 'T -> 'U -> bool) source1 source2 =
        ParallelEnumerable.All(
            ParallelEnumerable.Zip(toP (source1), toP (source2), Func<_, _, _>(predicate)),
            Func<_, _>(id)
        )

    let filter predicate source = ParallelEnumerable.Where(toP source, Func<_, _>(predicate))

    let iter action source = ParallelEnumerable.ForAll(toP source, Action<_>(action))

    let map mapping source = ParallelEnumerable.Select(toP source, new Func<_, _>(mapping))

    let pick chooser source =
        let projected = ParallelEnumerable.Select(toP source, Func<_, _>(chooser))

        let res = ParallelEnumerable.FirstOrDefault(projected, Func<_, _>(Option.isSome))

        match res with
        | Some x -> x
        | None -> raise (new System.Collections.Generic.KeyNotFoundException())

    let find predicate source = ParallelEnumerable.First(toP source, Func<_, _>(predicate))

    let tryFind predicate source =
        let withSomes = ParallelEnumerable.Select(toP source, Func<_, _>(Some))

        ParallelEnumerable.FirstOrDefault(withSomes, Func<_, _>(fun x -> predicate (Option.get (x))))

    let findIndex predicate source =
        let indexed = ParallelEnumerable.Select(toP source, Func<_, int, _>(fun x i -> (i, x)))

        match ParallelEnumerable.First(indexed, Func<_, _>(fun (_, x) -> predicate (x))) with
        | (i, x) -> i

    let tryFindIndex predicate source =
        let indexed = ParallelEnumerable.Select(toP source, Func<_, int, _>(fun x i -> Some(i, x)))

        match ParallelEnumerable.FirstOrDefault(indexed, Func<_, _>(fun x -> predicate (snd (Option.get x)))) with
        | Some (i, x) -> Some i
        | None -> None

    let ofArray (source: 'T array) = source.AsParallel()

    let toArray source = ParallelEnumerable.ToArray(toP source)

    let ofList (source: 'T list) = source.AsParallel()

    let toList (source: seq<'T>) = toP source |> List.ofSeq

    let ofSeq (source: seq<'T>) = source.AsParallel()

    let toSeq source = toP(source).AsSequential()

    let cast<'T> (source: System.Collections.IEnumerable) : pseq<'T> = ParallelEnumerable.Cast<'T>(source.AsParallel())

    let collect mapping source = ParallelEnumerable.SelectMany(toP source, Func<_, _>(fun x -> (mapping x) :> seq<'U>))

    let append source1 source2 = ParallelEnumerable.Concat(toP (source1), toP (source2))

    let init count initializer = ParallelEnumerable.Select(ParallelEnumerable.Range(0, count), Func<_, _>(initializer))

    let iter2 action source1 source2 =
        ParallelEnumerable.Zip(toP (source1), toP (source2), Func<_, _, _>(fun x y -> do action x y)) |> ignore

    let nth index source = ParallelEnumerable.ElementAt(toP source, index)

    let map2 mapping source1 source2 =
        ParallelEnumerable.Zip(toP (source1), toP (source2), Func<_, _, _>(fun x y -> mapping x y))

    let zip source1 source2 = ParallelEnumerable.Zip(toP (source1), toP (source2), Func<_, _, _>(fun x y -> (x, y)))

    let mapi mapping source = ParallelEnumerable.Select(toP source, new Func<_, _, _>(fun i c -> mapping c i))

    let iteri action source =
        let indexed = ParallelEnumerable.Select(toP source, Func<_, _, _>(fun x i -> (x, i)))

        ParallelEnumerable.ForAll(indexed, Action<_>(fun (x, i) -> action i x))

    let takeWhile predicate source = ParallelEnumerable.TakeWhile(toP source, Func<_, _>(predicate))

    let skip count source = ParallelEnumerable.Skip(toP source, count)

    let skipWhile predicate source = ParallelEnumerable.SkipWhile(toP source, Func<_, _>(predicate))

    let groupBy (projection: 'T -> 'Key) source =
        ParallelEnumerable.GroupBy(
            toP source,
            Func<_, _>(projection),
            Func<_, _, _>(fun k v -> (k, v)),
            HashIdentity.Structural<_>
        )

    let distinct source = ParallelEnumerable.Distinct(toP source, HashIdentity.Structural<_>)

    let distinctBy projection source =
        let comparer =
            { new System.Collections.Generic.IEqualityComparer<'T * 'Key> with
                member this.Equals(((_, p1)), ((_, p2))) = p1 = p2
                member this.GetHashCode((_, p1)) = p1.GetHashCode() }

        let projected = ParallelEnumerable.Select(toP source, Func<_, _>(fun x -> (x, projection x)))

        let distinct = ParallelEnumerable.Distinct(projected, comparer)

        ParallelEnumerable.Select(distinct, Func<_, _>(fun (x, px) -> x))

    let sort source =
        ParallelEnumerable.OrderBy(toP source, Func<_, _>(fun x -> x), ComparisonIdentity.Structural<_>) :> pseq<'T>

    let sortBy (projection: 'T -> 'Key) source =
        ParallelEnumerable.OrderBy(toP source, Func<_, _>(projection), ComparisonIdentity.Structural<_>) :> pseq<'T>

    let countBy projection source =
        ParallelEnumerable.GroupBy(toP source, Func<_, _>(projection), Func<_, _, _>(fun k vs -> (k, Seq.length vs)))

    let concat sources =
        ParallelEnumerable.Aggregate(toP (sources), empty, Func<_, _, _>(fun soFar y -> append soFar (toP y): pseq<_>))

    let choose chooser source =
        let projected = ParallelEnumerable.Select(toP source, Func<_, _>(chooser))

        let somes = ParallelEnumerable.Where(projected, Func<_, _>(Option.isSome))

        ParallelEnumerable.Select(somes, Func<_, _>(Option.get))

    let inline average
        (source: seq< ^T >)
        : ^T when ^T: (static member (+) : ^T * ^T -> ^T) and ^T: (static member DivideByInt : ^T * int -> ^T) and ^T: (static member Zero :
            ^T) =
        match source with
        | null -> nullArg "source"
        | :? seq<float> as source -> unbox (ParallelEnumerable.Average(toP source))
        | :? seq<float32> as source -> unbox (ParallelEnumerable.Average(toP source))
        | :? seq<decimal> as source -> unbox (ParallelEnumerable.Average(toP source))
        | _ ->
            failwithf
                "Average is supported for element types float, float32 and decimal, but given type : %source"
                (typeof<( ^T)>.ToString ())

    let inline averageBy
        (projection: 'T -> ^U)
        (source: seq<'T>)
        : ^U when ^U: (static member (+) : ^U * ^U -> ^U) and ^U: (static member DivideByInt : ^U * int -> ^U) and ^U: (static member Zero :
            ^U) =
        let bType = typeof<( ^U)>

        if bType = typeof<float> then
            unbox (ParallelEnumerable.Average(toP source, Func<_, float>(fun x -> unbox (projection x))))
        elif bType = typeof<float32> then
            unbox (ParallelEnumerable.Average(toP source, Func<_, float32>(fun x -> unbox (projection x))))
        elif bType = typeof<decimal> then
            unbox (ParallelEnumerable.Average(toP source, Func<_, decimal>(fun x -> unbox (projection x))))
        else
            failwithf
                "AverageBy is supported for projections to types float, float32 and decimal, but used at type type : %source"
                (bType.ToString())

    let inline sum
        (source: seq< ^T >)
        : ^T when ^T: (static member (+) : ^T * ^T -> ^T) and ^T: (static member Zero : ^T) =
        match source with
        | null -> nullArg "source"
        | :? seq<int> as source -> unbox (ParallelEnumerable.Sum(toP source))
        | :? seq<int64> as source -> unbox (ParallelEnumerable.Sum(toP source))
        | :? seq<float> as source -> unbox (ParallelEnumerable.Sum(toP source))
        | :? seq<float32> as source -> unbox (ParallelEnumerable.Sum(toP source))
        | :? seq<decimal> as source -> unbox (ParallelEnumerable.Sum(toP source))
        | _ ->
            failwithf
                "Sum is supported for element types int, int64, float, float32 and decimal, but given type : %source"
                (typeof<( ^T)>.ToString ())

    let inline sumBy
        (projection: 'T -> ^U)
        (source: seq<'T>)
        : ^U when ^U: (static member (+) : ^U * ^U -> ^U) and ^U: (static member Zero : ^U) =
        let bType = typeof<( ^U)>

        if bType = typeof<int> then
            unbox (ParallelEnumerable.Sum(toP source, Func<_, int>(fun x -> unbox (projection x))))
        elif bType = typeof<int64> then
            unbox (ParallelEnumerable.Sum(toP source, Func<_, int64>(fun x -> unbox (projection x))))
        elif bType = typeof<float> then
            unbox (ParallelEnumerable.Sum(toP source, Func<_, float>(fun x -> unbox (projection x))))
        elif bType = typeof<float32> then
            unbox (ParallelEnumerable.Sum(toP source, Func<_, float32>(fun x -> unbox (projection x))))
        elif bType = typeof<decimal> then
            unbox (ParallelEnumerable.Sum(toP source, Func<_, decimal>(fun x -> unbox (projection x))))
        else
            failwithf
                "SumBy is supported for projections to types int, int64, float, float32 and decimal, but given type : %source"
                (bType.ToString())

    let inline min (source: seq< ^T >) : ^T when ^T: comparison =
        match source with
        | null -> nullArg "source"
        | :? seq<int> as source -> unbox (ParallelEnumerable.Min(toP source))
        | :? seq<int64> as source -> unbox (ParallelEnumerable.Min(toP source))
        | :? seq<float> as source -> unbox (ParallelEnumerable.Min(toP source))
        | :? seq<float32> as source -> unbox (ParallelEnumerable.Min(toP source))
        | :? seq<decimal> as source -> unbox (ParallelEnumerable.Min(toP source))
        | _ -> ParallelEnumerable.Min(toP source, Func<_, _>(fun x -> x))

    let inline minBy (projection: ^T -> ^U) (source: seq< ^T >) : ^T when ^U: comparison =
        let elemsAndVals = ParallelEnumerable.Select(toP source, Func<_, _>(fun x -> projection x, x))

        let (_, elem) =
            ParallelEnumerable.Aggregate(
                elemsAndVals,
                Func<_, _, _> (fun (minVal, minElem) (curVal, curElem) ->
                    if curVal < minVal then (curVal, curElem) else (minVal, minElem))
            )

        elem

    let inline max (source: seq< ^T >) : ^T =
        match source with
        | null -> nullArg "source"
        | :? seq<int> as source -> unbox (ParallelEnumerable.Max(toP source))
        | :? seq<int64> as source -> unbox (ParallelEnumerable.Max(toP source))
        | :? seq<float> as source -> unbox (ParallelEnumerable.Max(toP source))
        | :? seq<float32> as source -> unbox (ParallelEnumerable.Max(toP source))
        | :? seq<decimal> as source -> unbox (ParallelEnumerable.Max(toP source))
        | _ -> ParallelEnumerable.Max(toP source, Func<_, _>(fun x -> x))

    let inline maxBy (projection: ^T -> ^U) (source: seq< ^T >) : ^T =
        let elemsAndVals = ParallelEnumerable.Select(toP source, Func<_, _>(fun x -> projection x, x))

        let (_, elem) =
            ParallelEnumerable.Aggregate(
                elemsAndVals,
                Func<_, _, _> (fun (minVal, minElem) (curVal, curElem) ->
                    if curVal > minVal then (curVal, curElem) else (minVal, minElem))
            )

        elem


    // Missing Seq.* functions
    //
    //    ?	zip3
    //    ?	scan
    //    ?	windowed
    //    ?	tryPick
    //    ?	take
    //    ?	readonly
    //    ?	pairwise
    //    ?	initInfinite
    //    ?	delay
    //    ?	compareWith
    //    ?	unfold


    // Parallel-specific functionality
    let ordered source = toP(source).AsOrdered()

    let withDegreeOfParallelism n source = toP(source).WithDegreeOfParallelism(n)

    let withExecutionMode executionMode source = toP(source).WithExecutionMode(executionMode)

    let withMergeOptions mergeOptions source = toP(source).WithMergeOptions(mergeOptions)

    let withCancellation cancellationToken source = toP(source).WithCancellation(cancellationToken)

    let cache source = toP (Seq.cache source)
