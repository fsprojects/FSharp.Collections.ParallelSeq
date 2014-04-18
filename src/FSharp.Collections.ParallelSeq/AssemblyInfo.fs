namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.Collections.ParallelSeq")>]
[<assembly: AssemblyProductAttribute("FSharp.Collections.ParallelSeq")>]
[<assembly: AssemblyDescriptionAttribute("Parallel sequence operators for F#.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
