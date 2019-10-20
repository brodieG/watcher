# watch - Watch State of Function Environment

## Overview

Tools to watch the state of a function environment as it evaluates each of its
component 'top-level' calls.

This is an experimental package built and tested primarily against a handful
of functions, so it is likely there are many unaddressed corner cases and
fragility.  In particular, we rely on empirically observed but undocumented
structure of the `getParseData` output.

The API may change completely in future iterations if there are any.  More
likely there will be no future updates to the project.  Feel free to file
issues.  I may or may not respond to them.

## Usage

A watched function will return it's result with a `watch.data` attribute
attached.  Consider the simple function:

```{r}
seq_sum <- function(n) {
  x <- 0
  for(i in seq_len(n)) {
    x <- x + i
  }
  x
}
seq_sum(4)
```

We can instrument it:

```{r}
seq_sum_w <- watcher::watch(seq_sum)
seq_sum_w
```

The semantics remain mostly unchanged, except that the return value gains
attributes with the watch data:

```{r}
res <- seq_sum_w(4)
watch.dat <- watcher::simplify_data(attr(res, 'watch.data'))
with(watch.dat$.scalar, plot(.id, x))
```

Each step of the function evaluation is recorded as a list element.
`simplify_data` will attempt to combine variables across steps.  For example,
scalar variables are turned into vectors and returned as members of the
".scalar" element of the resulting list.  In addition to the function variables
each step records a `.id` and `.line` scalar variable representing respectively
the evaluation step, and the line number in the function it corresponds to.
This can be used to line up code and state as in this animation:

INSERT GIF.

Unfortunately the process that produces the animation is still quite manual as
[you can see][].

## Possible Improvements

* Handle nested/recursive calls.  Conceptually this isn't too difficult: we just
  need to add a call that checks `.res` for the `watch.*` attributes after each
  top level statement.  Simplification / visualization will be another matter.
* Protect the symbols used in tracking (i.e. `.res`, `.line`, `.id`) to avoid
  conflicts with existing variables of the same name (or at least check for
  conflict).
* Optimize code; currently we've made no effort to make the code efficient.

## Acknowledgements

* Jim Hester for the instrumentation concept; if you are interested in writing
  your own instrumented code please base it off of his and not mine as I just
  threw something together quickly with little thought.
* Thomas Lin Pedersen for gganimate
* Hadley Wickham for Ggplot
