//=========================================================================
// (c) Microsoft Corporation 2005-2009.
//=========================================================================

namespace FSharp.Collections.ParallelSeq

open System
open System.Collections
open System.Collections.Generic
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections
open System.Linq
open System.Threading


type pseq<'T> = ParallelQuery<'T>

/// <summary>Parallel operations on IEnumerables.</summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PSeq =

    /// <summary>Operates in parallel, using System.Linq.Parallel. Wraps the two given enumerations as a single concatenated
    /// enumeration.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not be accessed
    /// concurrently.</remarks>
    ///
    /// <param name="source1">The first sequence.</param>
    /// <param name="source2">The second sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the two provided sequences is
    /// null.</exception>
    // [<CompiledName("Append")>]
    val append : source1: seq<'T> -> source2: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Wraps a loosely-typed System.Collections sequence as a typed sequence.</summary>
    ///
    /// <remarks>The use of this function usually requires a type annotation.
    /// An incorrect type annotation may result in runtime type
    /// errors.
    /// Individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Cast")>]
    val cast : source: IEnumerable -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to each element of the list. Return
    /// the list comprised of the results "x" for each element where
    /// the function returns Some(x).</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not
    /// be accessed concurrently.</remarks>
    ///
    /// <param name="chooser">A function to transform items of type T into options of type U.</param>
    /// <param name="source">The input sequence of type T.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Choose")>]
    val choose : chooser: ('T -> 'U option) -> source: seq<'T> -> pseq<'U>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to each element of the sequence and concatenates all the
    /// results.</summary>
    ///
    /// <remarks>Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
    ///
    /// <param name="mapping">A function to transform elements of the input sequence into the sequences
    /// that will then be concatenated.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Collect")>]
    val collect : mapping: ('T -> 'Collection) -> source: seq<'T> -> pseq<'U> when 'Collection :> seq<'U>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Combines the given enumeration-of-enumerations as a single concatenated
    /// enumeration.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="sources">The input enumeration-of-enumerations.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Concat")>]
    val concat : sources: seq<'Collection> -> pseq<'T> when 'Collection :> seq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies a key-generating function to each element of a sequence and return a sequence yielding unique
    /// keys and their number of occurrences in the original sequence.</summary>
    ///
    /// <remarks>Note that this function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences. The function makes no assumption on the ordering of the original
    /// sequence.</remarks>
    ///
    /// <param name="projection">A function transforming each item of input sequence into a key to be
    /// compared against the others.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("CountBy")>]
    val countBy : projection: ('T -> 'Key) -> source: seq<'T> -> pseq<'Key * int> when 'Key: equality

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that contains no duplicate entries according to generic hash and
    /// equality comparisons on the entries.
    /// If an element occurs multiple times in the sequence then the later occurrences are discarded.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Distinct")>]
    val distinct : source: seq<'T> -> pseq<'T> when 'T: equality

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that contains no duplicate entries according to the
    /// generic hash and equality comparisons on the keys returned by the given key-generating function.
    /// If an element occurs multiple times in the sequence then the later occurrences are discarded.</summary>
    ///
    /// <param name="projection">A function transforming the sequence items into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("DistinctBy")>]
    val distinctBy : projection: ('T -> 'Key) -> source: seq<'T> -> pseq<'T> when 'Key: equality

    /// <summary>Operates in parallel, using System.Linq.Parallel. Creates an empty sequence.</summary>
    ///
    /// <returns>The result sequence.</returns>
    // [<CompiledName("Empty")>]
    [<GeneralizableValueAttribute>]
    val empty<'T> : pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Tests if any element of the sequence satisfies the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input sequence. If any application
    /// returns true then the overall result is true and no further elements are tested.
    /// Otherwise, false is returned.</remarks>
    ///
    /// <param name="predicate">A function to test each item of the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Exists")>]
    val exists : predicate: ('T -> bool) -> source: seq<'T> -> bool

    /// <summary>Operates in parallel, using System.Linq.Parallel. Tests if any pair of corresponding elements of the input sequences satisfies the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to matching elements in the two sequences up to the lesser of the
    /// two lengths of the collections. If any application returns true then the overall result is
    /// true and no further elements are tested. Otherwise, false is returned. If one sequence is shorter than
    /// the other then the remaining elements of the longer sequence are ignored.</remarks>
    ///
    /// <param name="predicate">A function to test each pair of items from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the two input sequences is null.</exception>
    // [<CompiledName("Exists2")>]
    val exists2 : predicate: ('T1 -> 'T2 -> bool) -> source1: seq<'T1> -> source2: seq<'T2> -> bool

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a new collection containing only the elements of the collection
    /// for which the given predicate returns "true".</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.
    ///
    /// Remember sequence is lazy, effects are delayed until it is enumerated.</remarks>
    ///
    /// <param name="predicate">A function to test whether each item in the input sequence should be included in the output.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Filter")>]
    val filter : predicate: ('T -> bool) -> source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the first element for which the given function returns <c>true</c>.</summary>
    ///
    /// <param name="predicate">A function to test whether an item in the sequence should be returned.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if no element returns true when
    /// evaluated by the predicate</exception>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null</exception>
    // [<CompiledName("Find")>]
    val find : predicate: ('T -> bool) -> source: seq<'T> -> 'T

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the index of the first element for which the given function returns <c>true</c>.</summary>
    ///
    /// <param name="predicate">A function to test whether the index of a particular element should be returned.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if no element returns true when
    /// evaluated by the predicate</exception>
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null</exception>
    // [<CompiledName("FindIndex")>]
    val findIndex : predicate: ('T -> bool) -> source: seq<'T> -> int

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c>
    /// then computes <c>f (... (f source i0)...) iN</c></summary>
    ///
    /// <param name="folder">A function that updates the state with each element from the sequence.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Fold")>]
    val fold<'T, 'State> : folder: ('State -> 'T -> 'State) -> state: 'State -> source: seq<'T> -> 'State

    /// <summary>Operates in parallel, using System.Linq.Parallel. Tests if all elements of the sequence satisfy the given predicate.</summary>
    ///
    /// <remarks>The predicate is applied to the elements of the input sequence. If any application
    /// returns false then the overall result is false and no further elements are tested.
    /// Otherwise, true is returned.</remarks>
    ///
    /// <param name="predicate">A function to test an element of the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("ForAll")>]
    val forall : predicate: ('T -> bool) -> source: seq<'T> -> bool

    /// <summary>Operates in parallel, using System.Linq.Parallel. Tests the all pairs of elements drawn from the two sequences satisfy the
    /// given predicate. If one sequence is shorter than
    /// the other then the remaining elements of the longer sequence are ignored.</summary>
    ///
    /// <param name="predicate">A function to test pairs of elements from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
    // [<CompiledName("ForAll2")>]
    val forall2 : predicate: ('T1 -> 'T2 -> bool) -> source1: seq<'T1> -> source2: seq<'T2> -> bool

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies a key-generating function to each element of a sequence and yields a sequence of
    /// unique keys. Each unique key has also contains a sequence of all elements that match
    /// to this key.</summary>
    ///
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences. The function makes no assumption on the ordering of the original
    /// sequence.</remarks>
    ///
    /// <param name="projection">A function that transforms an element of the sequence into a comparable key.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    // [<CompiledName("GroupBy")>]
    val groupBy : projection: ('T -> 'Key) -> source: seq<'T> -> pseq<'Key * seq<'T>> when 'Key: equality

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the first element of the sequence.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input does not have any elements.</exception>
    // [<CompiledName("Head")>]
    val head : source: seq<'T> -> 'T

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns true if the sequence contains no elements, false otherwise.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("IsEmpty")>]
    val isEmpty : source: seq<'T> -> bool

    /// <summary>Operates in parallel, using System.Linq.Parallel. Generates a new sequence which, when iterated, will return successive
    /// elements by calling the given function, up to the given count.  The results of calling the function
    /// will not be saved, that is the function will be reapplied as necessary to
    /// regenerate the elements.  The function is passed the index of the item being
    /// generated.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="count">The maximum number of items to generate for the sequence.</param>
    /// <param name="initializer">A function that generates an item in the sequence from a given index.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentException">Thrown when count is negative.</exception>
    // [<CompiledName("Initialize")>]
    val init : count: int -> initializer: (int -> 'T) -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to each element of the collection.</summary>
    ///
    /// <param name="action">A function to apply to each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Iterate")>]
    val iter : action: ('T -> unit) -> source: seq<'T> -> unit

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to each element of the collection. The integer passed to the
    /// function indicates the index of element.</summary>
    ///
    /// <param name="action">A function to apply to each element of the sequence that can also access the current index.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("IterateIndexed")>]
    val iteri : action: (int -> 'T -> unit) -> source: seq<'T> -> unit

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to two collections simultaneously. If one sequence is shorter than
    /// the other then the remaining elements of the longer sequence are ignored.</summary>
    ///
    /// <param name="action">A function to apply to each pair of elements from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
    // [<CompiledName("Iterate2")>]
    val iter2 : action: ('T1 -> 'T2 -> unit) -> source1: seq<'T1> -> source2: seq<'T2> -> unit

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the lengthof the sequence</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Length")>]
    val length : source: seq<'T> -> int

    /// <summary>Operates in parallel, using System.Linq.Parallel. Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection.  The given function will be applied
    /// as elements are demanded using the <c>MoveNext</c> method on enumerators retrieved from the
    /// object.</summary>
    ///
    /// <remarks>The returned sequence may be passed between threads safely. However,
    /// individual IEnumerator values generated from the returned sequence should not be accessed concurrently.</remarks>
    ///
    /// <param name="mapping">A function to transform items from the input sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Map")>]
    val map : mapping: ('T -> 'U) -> source: seq<'T> -> pseq<'U>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Builds a new collection whose elements are the results of applying the given function
    /// to the corresponding pairs of elements from the two sequences. If one input sequence is shorter than
    /// the other then the remaining elements of the longer sequence are ignored.</summary>
    ///
    /// <param name="mapping">A function to transform pairs of items from the input sequences.</param>
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
    // [<CompiledName("Map2")>]
    val map2 : mapping: ('T1 -> 'T2 -> 'U) -> source1: seq<'T1> -> source2: seq<'T2> -> pseq<'U>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Builds a new collection whose elements are the results of applying the given function
    /// to each of the elements of the collection. The integer index passed to the
    /// function indicates the index (from 0) of element being transformed.</summary>
    ///
    /// <param name="mapping">A function to transform items from the input sequence that also supplies the current index.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("MapIndexed")>]
    val mapi : mapping: (int -> 'T -> 'U) -> source: seq<'T> -> pseq<'U>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Computes the nth element in the collection.</summary>
    ///
    /// <param name="index">The index of element to retrieve.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Get")>]
    val nth : index: int -> source: seq<'T> -> 'T

    // [<CompiledName("OfArray")>]
    /// <summary>Operates in parallel, using System.Linq.Parallel. Views the given array as a sequence.</summary>
    ///
    /// <param name="source">The input array.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    val ofArray : source: 'T array -> pseq<'T>

    // [<CompiledName("OfList")>]
    /// <summary>Operates in parallel, using System.Linq.Parallel. Views the given list as a sequence.</summary>
    ///
    /// <param name="source">The input list.</param>
    ///
    /// <returns>The result sequence.</returns>
    val ofList : source: 'T list -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies the given function to successive elements, returning the first
    /// <c>x</c> where the function returns "Some(x)".</summary>
    ///
    /// <param name="chooser">A function to transform each item of the input sequence into an option of the output type.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when every item of the sequence
    /// evaluates to <c>None</c> when the given function is applied.</exception>
    // [<CompiledName("Pick")>]
    val pick : chooser: ('T -> 'U option) -> source: seq<'T> -> 'U

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies a function to each element of the sequence, threading an accumulator argument
    /// through the computation. Begin by applying the function to the first two elements.
    /// Then feed this result into the function along with the third element and so on.
    /// Return the final result.</summary>
    ///
    /// <param name="reduction">A function that takes in the current accumulated result and the next
    /// element of the sequence to produce the next accumulated result.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    // [<CompiledName("Reduce")>]
    val reduce : reduction: ('T -> 'T -> 'T) -> source: seq<'T> -> 'T

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that yields one item only.</summary>
    ///
    /// <param name="value">The input item.</param>
    ///
    /// <returns>The result sequence.</returns>
    // [<CompiledName("Singleton")>]
    val singleton : value: 'T -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that skips N elements of the underlying sequence and then yields the
    /// remaining elements of the sequence.</summary>
    ///
    /// <param name="count">The number of items to skip.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.InvalidOperationException">Thrown when count exceeds the number of elements
    /// in the sequence.</exception>
    // [<CompiledName("Skip")>]
    val skip : count: int -> source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that, when iterated, skips elements of the underlying sequence while the
    /// given predicate returns <c>true</c>, and then yields the remaining elements of the sequence.</summary>
    ///
    /// <param name="predicate">A function that evaluates an element of the sequence to a boolean value.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("SkipWhile")>]
    val skipWhile : predicate: ('T -> bool) -> source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Yields a sequence ordered by keys.</summary>
    ///
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences. The function makes no assumption on the ordering of the original
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Sort")>]
    val sort : source: seq<'T> -> pseq<'T> when 'T: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Applies a key-generating function to each element of a sequence and yield a sequence ordered
    /// by keys.  The keys are compared using generic comparison as implemented by <c>Operators.compare</c>.</summary>
    ///
    /// <remarks>This function returns a sequence that digests the whole initial sequence as soon as
    /// that sequence is iterated. As a result this function should not be used with
    /// large or infinite sequences. The function makes no assumption on the ordering of the original
    /// sequence.
    ///
    /// This is a stable sort, that is the original order of equal elements is preserved.</remarks>
    ///
    /// <param name="projection">A function to transform items of the input sequence into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("SortBy")>]
    val sortBy : projection: ('T -> 'Key) -> source: seq<'T> -> pseq<'T> when 'Key: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that, when iterated, yields elements of the underlying sequence while the
    /// given predicate returns <c>true</c>, and then returns no further elements.</summary>
    ///
    /// <param name="predicate">A function that evaluates to false when no more items should be returned.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("TakeWhile")>]
    val takeWhile : predicate: ('T -> bool) -> source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Builds an array from the given collection.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("ToArray")>]
    val toArray : source: seq<'T> -> 'T array

    /// <summary>Operates in parallel, using System.Linq.Parallel. Builds a list from the given collection.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("ToList")>]
    val toList : source: seq<'T> -> 'T list

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the first element for which the given function returns <c>true</c>.
    /// Return <c>None</c> if no such element exists.</summary>
    ///
    /// <param name="predicate">A function that evaluates to a Boolean when given an item in the sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("TryFind")>]
    val tryFind : predicate: ('T -> bool) -> source: seq<'T> -> 'T option

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the index of the first element in the sequence
    /// that satisfies the given predicate. Return <c>None</c> if no such element exists.</summary>
    ///
    /// <param name="predicate">A function that evaluates to a Boolean when given an item in the sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("TryFindIndex")>]
    val tryFindIndex : predicate: ('T -> bool) -> source: seq<'T> -> int option

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that when enumerated returns at most N elements.</summary>
    ///
    /// <param name="count">The maximum number of items to enumerate.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    // [<CompiledName("Truncate")>]
    val truncate : count: int -> source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Combines the two sequences into a list of pairs. The two sequences need not have equal lengths:
    /// when one sequence is exhausted any remaining elements in the other
    /// sequence are ignored.</summary>
    ///
    /// <param name="source1">The first input sequence.</param>
    /// <param name="source2">The second input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when either of the input sequences is null.</exception>
    // [<CompiledName("Zip")>]
    val zip : source1: seq<'T1> -> source2: seq<'T2> -> pseq<'T1 * 'T2>

    val ordered : source: seq<'T> -> pseq<'T>

    val withDegreeOfParallelism : n: int -> source: seq<'T> -> pseq<'T>

    val withExecutionMode : executionMode: ParallelExecutionMode -> source: seq<'T> -> pseq<'T>

    val withMergeOptions : mergeOptions: ParallelMergeOptions -> source: seq<'T> -> pseq<'T>

    val withCancellation : cancellationToken: CancellationToken -> source: seq<'T> -> pseq<'T>
    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the sum of the elements in the sequence.</summary>
    ///
    /// <remarks>The elements are summed using the <c>+</c> operator and <c>Zero</c> property associated with the generated type.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    //[<CompiledName("Sum")>]
    val inline sum : source: seq<(^T)> -> ^T
        when ^T: (static member (+) : ^T * ^T -> ^T) and ^T: (static member Zero : ^T)

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the sum of the results generated by applying the function to each element of the sequence.</summary>
    /// <remarks>The generated elements are summed using the <c>+</c> operator and <c>Zero</c> property associated with the generated type.</remarks>
    ///
    /// <param name="projection">A function to transform items from the input sequence into the type that will be summed.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    //[<CompiledName("SumBy")>]
    val inline sumBy : projection: ('T -> ^U) -> source: seq<'T> -> ^U
        when ^U: (static member (+) : ^U * ^U -> ^U) and ^U: (static member Zero : ^U)

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the lowest of all elements of the sequence, compared via <c>Operators.min</c>.</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    //[<CompiledName("Min")>]
    val inline min : source: seq<(^T)> -> ^T when ^T: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the lowest of all elements of the sequence, compared via Operators.min on the function result.</summary>
    ///
    /// <param name="projection">A function to transform items from the input sequence into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    //[<CompiledName("MinBy")>]
    val inline minBy : projection: (^T -> ^U) -> source: seq<(^T)> -> ^T when ^U: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the greatest of all elements of the sequence, compared via Operators.max</summary>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    ///
    /// <returns>The result sequence.</returns>
    //[<CompiledName("Max")>]
    val inline max : source: seq<(^T)> -> ^T when ^T: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the greatest of all elements of the sequence, compared via Operators.max on the function result.</summary>
    ///
    /// <param name="projection">A function to transform items from the input sequence into comparable keys.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence is empty.</exception>
    //[<CompiledName("MaxBy")>]
    val inline maxBy : projection: (^T -> ^U) -> source: seq<(^T)> -> ^T when ^U: comparison

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns a sequence that corresponds to a cached version of the input sequence.
    /// This result sequence will have the same elements as the input sequence. The result
    /// can be enumerated multiple times. The input sequence will be enumerated at most
    /// once and only as far as is necessary.  Caching a sequence is typically useful when repeatedly
    /// evaluating items in the original sequence is computationally expensive or if
    /// iterating the sequence causes side-effects that the user does not want to be
    /// repeated multiple times.
    ///
    /// Enumeration of the result sequence is thread safe in the sense that multiple independent IEnumerator
    /// values may be used simultaneously from different threads (accesses to
    /// the internal lookaside table are thread safe). Each individual IEnumerator
    /// is not typically thread safe and should not be accessed concurrently.</summary>
    ///
    /// <remarks>Once enumeration of the input sequence has started,
    /// it'source enumerator will be kept live by this object until the enumeration has completed.
    /// At that point, the enumerator will be disposed.
    ///
    /// The enumerator may be disposed and underlying cache storage released by
    /// converting the returned sequence object to type IDisposable, and calling the Dispose method
    /// on this object. The sequence object may then be re-enumerated and a fresh enumerator will
    /// be used.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    //[<CompiledName("Cache")>]
    val cache : source: seq<'T> -> pseq<'T>

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the average of the elements in the sequence.</summary>
    ///
    /// <remarks>The elements are averaged using the <c>+</c> operator, <c>DivideByInt</c> method and <c>Zero</c> property
    /// associated with the element type.</remarks>
    ///
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence has zero elements.</exception>
    //[<CompiledName("Average")>]
    val inline average : source: seq<(^T)> -> ^T
        when ^T: (static member (+) : ^T * ^T -> ^T) and ^T: (static member DivideByInt : ^T * int -> ^T) and ^T: (static member Zero :
            ^T)

    /// <summary>Operates in parallel, using System.Linq.Parallel. Returns the average of the results generated by applying the function to each element
    /// of the sequence.</summary>
    ///
    /// <remarks>The elements are averaged using the <c>+</c> operator, <c>DivideByInt</c> method and <c>Zero</c> property
    /// associated with the generated type.</remarks>
    ///
    /// <param name="projection">A function applied to transform each element of the sequence.</param>
    /// <param name="source">The input sequence.</param>
    ///
    /// <returns>The result sequence.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>
    /// <exception cref="System.ArgumentException">Thrown when the input sequence has zero elements.</exception>
    //[<CompiledName("AverageBy")>]
    val inline averageBy : projection: ('T -> ^U) -> source: seq<'T> -> ^U
        when ^U: (static member (+) : ^U * ^U -> ^U) and ^U: (static member DivideByInt : ^U * int -> ^U) and ^U: (static member Zero :
            ^U)
