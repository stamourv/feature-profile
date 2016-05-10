#lang scribble/manual

@(require (for-label feature-profile feature-profile/plug-in-lib racket/base)
          scribble/eval racket/sandbox racket/port)

@title{Feature-Specific Profiling}

@defmodule[feature-profile]

This package provides experimental support for profiling the costs of specific
language and library features.

Unlike Racket's regular profiler, which reports time spent in each function,
the feature-specific profiler reports time spent in @emph{feature instances}:
a particular pattern matching form, a specific contract, etc.
This pinpoints which uses of expensive features are the sources of performance
issues and should be replaced by less expensive equivalents. Conversely, the
feature-specific profiler also reports which feature instances are not
problematic and should be left alone.

Out of the box, the profiler includes plug-ins for the following features:
@itemlist[
@item{Contracts} @; TODO point to `contract-profile'
@item{Output}
@item{Generic sequence dispatch}
@item{Typed Racket casts and assertions}
@item{Pattern matching}
@item{Method dispatch}
@item{Optional and keyword arguments}
]

Libraries may supply additional plug-ins for the features they introduce.
For example, the Marketplace @; TODO link
library includes @; TODO not released yet
a process accounting plug-in.

@; TODO point to paper when it's out

@section{Quick Start}

@index["raco contract-profile"]{
The simplest way to use this tool is to use the @exec{raco feature-profile}
command, which takes a file name as argument, and runs the feature-specific
profiler on the @racket[main] submodule of that file (if it exists), or on the
module itself (if there is no @racket[main] submodule).
}

Alternatively, wrap the code you wish to profile with the
@racket[feature-profile] form. Then, execute the program with the
@racket[feature-profile-compile-handler] active.

@margin-note{Some features, such as contracts, can be profiled without using
the compile handler. To get information about all features, the compile handler
is required.}

This can be accomplished by
running the program using @commandline{racket -l feature-profile -t program.rkt}
Note that any compiled files (with extension @tt{.zo}) should be removed before
running.

Here is an example feature profile:

@racketmod[#:file "fizzbuzz.rkt"
racket

(require feature-profile)

(define (divisible x n)
  (= 0 (modulo x n)))

(define (fizzbuzz n)
  (for ([i (range n)])
    (cond [(divisible i 15) (printf "FizzBuzz\n")]
          [(divisible i 5)  (printf "Buzz\n")]
          [(divisible i 3)  (printf "Fizz\n")]
          [else             (printf "~a\n" i)])))
(feature-profile
   (parameterize ([current-output-port (open-output-nowhere)])
      (fizzbuzz 10000000)))
]

@verbatim{
513 samples


Output
account(s) for 54.7% of total running time
4240 / 7752 ms

Cost Breakdown
  3354 ms : fizzbuzz.rkt:13:28
  620 ms : fizzbuzz.rkt:12:28
  134 ms : fizzbuzz.rkt:11:28
  132 ms : fizzbuzz.rkt:10:28


Generic Sequences
account(s) for 14.47% of total running time
1122 / 7752 ms

Cost Breakdown
  1122 ms : fizzbuzz.rkt:7:11
}

@margin-note{Some of the reports for contracts are produced in separate
files. See @secref["contract-profiling" #:doc '(lib "contract-profile/scribblings/contract-profile.scrbl")].}
The first part of the profile reports the time spent performing output, and
lists each call to @racket[printf] separately.
The second part reports the time spent doing generic sequence dispatch.
The @racket[for] form on line 5 operates generically on the list returned by
@racket[range], which introduces dispatch.


@section{Using the Profiler}


@defform[(feature-profile [#:features features] body ...)
         #:contracts ([features (listof feature?)])]{

Reports costs of feature instances and the overall costs of each feature to
@racket[current-output-port].

For each profiled feature, reports how much time was spent in the feature
overall, then breaks down that time by feature instance.
Features are sorted in decreasing order of time, and only features for which
time was observed are displayed.

The optional @racket[features] argument contains the lists of features that
should be observed by the feature profiler.
It defaults to @racket[default-features].
}

@defproc[(feature-profile-thunk [#:features features (listof feature?)]
                                [thunk (-> any)])
         any]{
Like @racket[feature-profile], but as a function which takes a thunk to profile
as argument.
}

@defproc[(feature-profile-compile-handler [stx any/c] [immediate-eval? boolean?]) compiled-expression?]{
Compiles @racket[stx] for feature profiling. This adds intrumentation around
feature code, which introduces a slight overhead (less than 20%).
}

@defthing[default-features (listof feature?)]{
The default set of features observed by the profiler.
}


@subsection{Default features}

@defmodule[feature-profile/features]
@deftogether[(@defthing[contracts-feature feature?]
              @defthing[output-feature feature?]
              @defthing[generic-sequence-dispatch-feature feature?]
              @defthing[type-casts-feature feature?]
              @defthing[keyword-optional-arguments-feature feature?]
              @defthing[pattern-matching-feature feature?]
              @defthing[send-dispatch-feature feature?])]{
Individual features provided by the feature profiler.
To use all of the features, use @racket[default-features].
}


@section{Adding Profiling Support to Libraries}

Not all expensive features come from the standard library; some are provided by
third-party libraries. For this reason, feature-specific profiling support for
third-party library features can be useful.

Adding feature-specific profiling for a library requires implementing a
feature-specific plug-in and adding feature marks to the library's
implementation (see @secref{instrumentation}).


@subsection[#:tag "instrumentation"]{Feature Instrumentation}

Implementing feature-specific profiling support for a library requires
instrumenting its implementation. Specifically, library code must arrange to
have @emph{feature marks} on the stack whenever feature code is running.
This is usually a non-intrusive change, and does not change program behavior.
Feature marks allow the profiler's sampling thread to observe when programs are
executing feature code.

Conceptually, feature marks are conditionally enabled
@techlink[#:doc '(lib "scribblings/reference/reference.scrbl")]{continuation marks}
that map feature keys to payloads.
Feature keys can be any value, but must uniquely identify features and must be
consistent with with the feature's plug-in (see @secref{plug-ins}).

Payloads can also be any value (except @racket[#f]), but they should uniquely
identify feature instances (e.g. a specific pattern matching form). The source
location of feature instances is an example of a good payload. Payloads with
additional information can be useful when implementing more sophisticated
feature-specific analyses (see @secref{analysis}).

To avoid attributing time to a feature when feature code transfers control to
user code (e.g. when a feature calls a function that was provided by the user),
you can install @emph{antimarks} which are feature marks with the
@racket['antimark] symbol as payload. Antimarks are recognized specially by the
profiling library, and any sample taken while an antimark is the most recent
feature mark will not contribute time toward that feature.

Feature marks come in three flavors, each with different tradeoffs and
appropriate for different use cases.

@itemlist[
@item{@emph{Active marks} correspond directly to continuation marks. Feature
code can be instrumented with active marks by wrapping it in
@racketblock[(with-continuation-mark _feature-key _payload _body)]

Active marks are the simplest flavor of feature marks, and do not require
recompiling programs (with @racket[feature-profile-compile-handler]) before
profiling. On the other hand, they impose a performance overhead even when not
profiling, and must be used with caution.
}

@item{@emph{Syntactic latent marks} are
@techlink[#:doc '(lib "scribblings/reference/reference.scrbl")]{syntax properties} attached to the
syntax of feature code, that are turned into actual continuation marks by
@racket[feature-profile-compile-handler] or a similar compile handler that
supports the key (see @racket[make-latent-mark-compile-handler]).

To instrument feature code with syntactic latent marks, syntax objects
corresponding to feature code must be wrapped with
@racketblock[(syntax-property _stx _feature-key _payload)]

If the payload is @racket[#f], the compile handler will automatically use the
syntax object's source location as payload.

Syntactic latent marks, unlike active marks, require recompilation but do not
impose overhead unless activated by the compile handler (which is typically
only done when profiling). Because of their reliance on syntax properties, they
are only appropriate for features implemented as syntactic extensions.
}

@item{@emph{Functional latent marks} are functions that are recognized
specially and whose calls are wrapped in continuation marks by a latent mark
compile handler. Functional latent marks make it possible to instrument
features that are implemented as functions, while avoiding the overhead of
active marks when not profiling.

For more details on how to use functional latent marks, see
@racket[make-latent-mark-compile-handler].

Compile handlers automatically use source locations as payloads for functional
latent marks.
}

]

@defproc[(make-latent-mark-compile-handler [latent-mark-keys (listof any/c)]
                                           [functional-latent-marks dict?])
         (-> any/c boolean? compiled-expression?)]{

Creates a compile handler similar to @racket[feature-profile-compile-handler]
that supports the provided kinds of latent marks.
Compile handlers produced by @racket[make-latent-mark-compile-handler] turn
latent marks with keys it recognizes into continuation marks, allowing the
corresponding features to be profiled.

The first argument is the list of feature keys used by the latent marks that
should be activated. It should usually be an extension of
@racket[default-syntactic-latent-mark-keys].

The second argument is a dictionary mapping the names of functions whose calls should
be profiled to the key of the feature that the function corresponds to.
It should usually be an extension of @racket[default-functional-latent-marks].
}

@defthing[default-syntactic-latent-mark-keys (listof any/c)]{
The list of syntactic latent mark keys used by features supported out of the
box by the feature-specific profiler.
}

@defthing[default-functional-latent-marks dict?]{
Dictionary mapping feature-related functions profiled by default to the
feature keys their marks should use.
}

@; TODO interface for compile-handler is inconsistent with entry point. have to
@; pass all features vs just the extra ones. may not be too bad. writing new
@; profilers is a less frequent thing

@subsection[#:tag "plug-ins"]{Implementing Feature Plug-Ins}

@defmodule[feature-profile/plug-in-lib]

At its core, a plug-in is a @racket[feature] struct.

@defstruct[feature ([name string?] [key any/c]
                    [grouper (or/c #f (-> any/c any/c))]
                    [analysis (or/c #f (-> feature-profile? any))])]{

@racket[name] is a string representing the user-facing name of the feature, and
is printed in the profile.

@racket[key] is the continuation mark key used by the feature's feature
marks. It can be any value, but must be consistent with the key used by the
feature's instrumentation.

@racket[grouper] is a function that should be used to group mark payloads that
correspond to a single feature instance. The function takes a payload as
argument, and returns a value that identifies the payload's equivalence class.
That is, all payloads that should be grouped together must return the same
value. The grouping function only considers the payload of the most recent mark
for a given feature.

A value of @racket[#f] will result in the plug-in using the default grouping
functions, which is usually what you want. The default function expects that the
payloads will be a vector holding the fields of a @racket[srcloc].

@racket[analysis] is a function that performs feature-specific analysis to
present profile results in a custom format. It is expected to produce its
results as a side-effect. Writing analysis functions is covered in
@secref{analysis}.

A value of @racket[#f] will result in the plug-in using the default analysis
provided by the profiling library. This analysis groups costs by feature
instance and prints them to standard output. It is usually best to use this
option to start with, and eventually migrate to a more sophisticated analysis.

}

The most basic feature plug-in is a feature struct with a name and a key, and
uses the default grouping and analysis.
To include the new feature when profiling, pass the feature struct to
@racket[feature-profile]'s or @racket[feature-profile-thunk]'s
@racket[#:extra-features] argument.


@subsection[#:tag "analysis"]{Implementing Feature-Specific Analyses}

@defmodule[feature-profile/plug-in-lib #:link-target? #f]

While the basic analysis provided by default by the profiling library (grouping
costs by feature instance) is useful, instances of some features (such as
contracts) carry enough information to make further analysis worthwhile.
Further analysis can be used to produce precise reports tailored specifically
to a given feature.

This section describes the API provided by the profiling library for building
feature-specific analyses.
This API is still experimental, and is likely to change in the future.

A feature-specific analysis is a function that takes a @racket[feature-report]
structure as an argument and emits profiling reports as a side effect.
This function should be used as the @racket[analysis] field of the relevant
@racket[feature] object.

@defstruct[feature-report ([feature feature?]
                           [core-samples (listof (listof any/c))]
                           [raw-samples any/c]
                           [grouped-samples (listof (listof (cons/c any/c any/c)))]
                           [total-time exact-nonnegative-integer?]
                           [feature-time exact-nonnegative-integer?])]{

Feature report structures contain information about a feature gathered during
program execution. Information is available in raw form (what's directly
gathered via sampling) and at various stages of processing by the core
profiler. This information can be used by feature-specific analyses to generate
custom reports and views.

The @racket[feature] field contains the @racket[feature] structure that
corresponds to the feature being profiled and analyzed.

The @racket[core-samples] field contains feature @emph{core samples}, that is
each sample is a list of all marks related to the feature of interest that are
on the stack at the time the sample is taken. The most recent mark is at the
front of the list. These core samples include antimarks.

The @racket[raw-samples] field contains the samples collected by the regular
Racket profiler during program execution, in the (intentionally) undocumented
format used by the profiler. These samples can be passed to the regular
profiler's analyzer, and the result correlated with feature data.

The @racket[grouped-samples] field contains a list of groups (lists) of
samples, grouped using the feature's grouping function. Each sample has the
most recent payload as its @racket[car] and a sample in the regular profiler's
format as its @racket[cdr].

The @racket[total-time] field contains the total time (in milliseconds)
observed by the sampler.

The @racket[feature-time] field contains the time (in milliseconds) for which a
feature mark was observed, as estimated by interpolating sample timestamps.
}


This library also provides helper functions for common analysis tasks.

@defproc[(print-feature-profile [f-p feature-report?]) void?]{
Displays the results of the built-in basic analysis. Useful when the custom
analysis for a feature supplements (but does not completely replace) basic
analysis.
}

@defproc[(make-interner) (-> (cons/c any/c any/c) any/c)]{
Produces an interning function that takes a name-location pair (e.g. from the
regular profiler's samples) and interns it. The regular profiler's analysis
requires name-location pairs to be interned, hence this function.
@; TODO move to the marketplace plug-in
}
