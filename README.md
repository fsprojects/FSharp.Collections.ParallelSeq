FSharp.Collections.ParallelSeq
==============================

Parallel (multi-core) sequence operations. See the documentation: http://fsprojects.github.io/FSharp.Collections.ParallelSeq/


### Maintainer(s)

- [@mydogisbox](https://github.com/mydogisbox)

The default maintainer account for projects under "fsprojects" is [@fsgit](https://github.com/fsgit) - F# Community Project Incubation Space (repo management)

### Dev Guide

To build and test:

    build.cmd RunTests
   
To make a release:

    build.cmd Release 
    .nuget\nuget.exe push bin\<the-nuget-package>  API-KEY
    
If you are a maintainer don't have the necessary permissions to push, then [add an admin issue](https://github.com/fsprojects/FsProjectsAdmin/issues) giving your nuget ID.

To update the docs:

    build.cmd ReleaseDocs
