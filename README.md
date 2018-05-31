[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.Collections.ParallelSeq/badge/issue)](http://issuestats.com/github/fsprojects/FSharp.Collections.ParallelSeq)
[![Issue Stats](http://issuestats.com/github/fsprojects/FSharp.Collections.ParallelSeq/badge/pr)](http://issuestats.com/github/fsprojects/FSharp.Collections.ParallelSeq)

FSharp.Collections.ParallelSeq
==============================

Parallel (multi-core) sequence operations. See the documentation: http://fsprojects.github.io/FSharp.Collections.ParallelSeq/

[![Build status](https://ci.appveyor.com/api/projects/status/7uaow0us61r19ox7/branch/master?svg=true)](https://ci.appveyor.com/project/fsprojectsgit/fsharp-collections-parallelseq/branch/master) (Windows)


[![NuGet Status](https://buildstats.info/nuget/FSharp.Collections.ParallelSeq)](https://www.nuget.org/packages/FSharp.Collections.ParallelSeq/)

### Maintainer(s)

- [@ChrSteinert](https://github.com/ChrSteinert)
- [@jamessdixon](https://github.com/jamessdixon)
- [@kapilash](https://github.com/kapilash)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)

### Dev Guide

To build and test:

    build.cmd RunTests
   
To make a release:

    build.cmd Release 
    set APIKEY=...
    .paket\paket.exe push src\FSharp.Collections.ParallelSeq\bin\Release\FSharp.Collections.ParallelSeq.1.1.2.nupkg --api-key %APIKEY% --url https://nuget.org
    
If you are a maintainer don't have the necessary permissions to push, then [add an admin issue](https://github.com/fsprojects/FsProjectsAdmin/issues) giving your nuget ID.

To update the docs:

    build.cmd ReleaseDocs
