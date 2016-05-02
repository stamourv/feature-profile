feature-profile
===============

This package provides support for profiling the costs of specific
features of the Racket language and standard library. This profiler
reports time spent in individual feature instances.

Out of the box, the profiler includes plug-ins for the following features:

* Contracts
* Output
* Generic sequence dispatch
* Typed Racket casts and assertions
* Pattern matching
* Method dispatch
* Optional and keyword arguments

Libraries may supply additional plug-ins for the features they introduce.

How to install
* `raco pkg install feature-profile`


How to use
* Wrap the code you want to profile in `feature-profile`
* Run it with `racket -l feature-profile -t program.rkt`
* `raco doc feature-profile`


[Docs](http://pkg-build.racket-lang.org/doc/feature-profile@feature-profile/index.html)
