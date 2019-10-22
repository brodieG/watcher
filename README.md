<!-- README.md is generated from README.Rmd. Please edit that file 

library(rmarkdown)
render('README.Rmd', output_format=md_document())

-->
watcher - Record Function State During Evaluation
=================================================

Overview
--------

A mechanism to record function state during evaluation for later
analysis. Variable values and meta data are recorded for each
‘top-level’ call.

This is package is experimental and rough around the edges, and was
built with expediency rather than robustness as the guiding principle.
The API may change completely in future iterations if there are any.
More likely there will be no future updates to the project, except in
the unlikely case it garners substantial attention.

Usage
-----

`watcher` works by “instrumenting” a target function. Consider a simple
function that adds the integers up to `n`:

    seq_sum <- function(n) {
      x <- 0
      for(i in seq_len(n)) {
        x <- x + i
      }
      x
    }
    res0 <- seq_sum(10)
    res0
    ## [1] 55

We can instrument it with `watch`:

    seq_sum_w <- watcher::watch(seq_sum)
    seq_sum_w
    ## function (n) 
    ## {
    ##     watcher::watch_init(character(0))
    ##     res <- {
    ##         {
    ##             .res <- (x <- 0)
    ##             watcher::capture_data(environment(), 2L)
    ##             .res
    ##         }
    ##         for (i in seq_len(n)) {
    ##             {
    ##                 .res <- (x <- x + i)
    ##                 watcher::capture_data(environment(), 4L)
    ##                 .res
    ##             }
    ##         }
    ##         {
    ##             .res <- (x)
    ##             watcher::capture_data(environment(), 6L)
    ##             .res
    ##         }
    ##     }
    ##     attr(res, "watch.data") <- watcher::watch_data()
    ##     attr(res, "watch.code") <- c("function(n) {", "  x <- 0", 
    ##     "  for(i in seq_len(n)) {", "    x <- x + i", "  }", "  x", 
    ##     "}")
    ##     res
    ## }

Each statement is wrapped in brackets, the result is recorded in `.res`,
and state is recorded with `capture_data`. This is messy as we add the
`.res` symbol to the function environment, and modify the return value
by adding attributes to it, but it worked for my purposes, particularly
because I wanted to record the environment immediately after each
statement[1].

The function semantics are mostly unchanged:

    res1 <- seq_sum_w(10)
    all.equal(res0, res1, check.attributes=FALSE)
    ## [1] TRUE

Except that the result is augmented with attributes with the watch data:

    watch.dat0 <- attr(res1, 'watch.data')
    str(watch.dat0[1:2])
    ## List of 2
    ##  $ :List of 2
    ##   ..$ x: num 0
    ##   ..$ n: num 10
    ##   ..- attr(*, "line")= int 2
    ##  $ :List of 3
    ##   ..$ i: int 1
    ##   ..$ x: num 1
    ##   ..$ n: num 10
    ##   ..- attr(*, "line")= int 4

Each step of the function evaluation is recorded as a list element. This
is a bit awkward to deal with so we can use `simplify_data` to make the
data more accessible. For example, scalar variables are turned into
vectors and returned as members of the “.scalar” element of the
simplified list. In addition to the function variables each step records
a `.id` and `.line` scalar variable representing respectively the
evaluation step, and the line number in the function it corresponds to.

    watch.dat1 <- watcher::simplify_data(watch.dat0)
    with(watch.dat1$.scalar, plot(.id, x))

![](extra/figures/README-state-vs-id-1.png)

With a little work we can combine the line information with the function
data to juxtapose function state with the code:

![](extra/sort-2.gif)

Unfortunately the process that produces the animation is still quite
manual as [you can see](extra/sort-2.R).

Possible Improvements
---------------------

-   Handle nested/recursive calls. Conceptually this isn’t too
    difficult: we just need to add a call that checks `.res` for the
    `watch.*` attributes after each top level statement. Simplification
    / visualization will be another matter.
-   Protect the symbols used in tracking (i.e. `.res`, `.line`, `.id`)
    to avoid conflicts with existing variables of the same name (or at
    least check for conflict).
-   Optimize code; currently we’ve made no effort to make the code
    efficient.

Acknowledgements
----------------

-   [Jim Hester](https://github.com/jimhester/) for the instrumentation
    concept which I borrowed from
    [`covr`](https://github.com/r-lib/covr); if you are interested in
    writing your own instrumented code please base it off of his and not
    mine as I just threw something together quickly with little thought.
-   [Thomas Lin Pedersen](https://github.com/thomasp85) for
    [`gganimate`](https://github.com/thomasp85/gganimate/) which I used
    for the insertion sort animation.
-   [Hadley Wickham](https://github.com/hadley/) for
    [`ggplot`](https://github.com/tidyverse/ggplot2).

[1] Obviously this could also be done without `.res`, but again,
whatever.
