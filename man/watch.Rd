% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/watch.R
\name{watch}
\alias{watch}
\title{Instrument a Function for Variable Watching}
\usage{
watch(fun, vars = character())
}
\arguments{
\item{fun}{a function to watch}

\item{vars}{character a vector of names of variables to record}
}
\value{
an instrumented version of \code{fun}.  When this instrumented function is
run it will add attributes "watch.data" and "watch.code" to the result.
See the description for details about return data format of the
instrumented function.
}
\description{
The input function will be modified such that the state of the function
environment is captured after each top-level statement is evaluated.
Top-level statements are, roughly speaking, distinct R expressions that are
visible directly in the source of the R function.
}
\details{
For each top-level step in the evaluation of a watched function a list
element is added to the "watch.data" attribute of the result.  The list will
contain a copy of all the variables in the function environment right after
that step is evaluated, or of the subset of them specified by the \code{vars}
parameter.  Additionally, the list will contain a "line" attribute that maps
to the start line of the top-level expression that was last evaluated.  This
line number maps to the deparsed function attached to the result as the
"watch.code" attribute.  The line numbers may or may not match to other
deparsings of the function, so if you intend on using the line numbers be
sure to do so in relation to the "watch.code" version of the function.

Instrumented function semantics should be the same as the un-instrumented
version, except for the attributes attached to the return value, and for the
use of the \code{.res} temporary variable.  If a function also uses the \code{.res}
symbol the instrumentation will interfere with the original semantics.
}
\note{
if you are watching a function from a package you might want to install
the package with \code{Sys.setenv('R_KEEP_PKG_SOURCE'='yes')} so that the line
numbers match the original source, although keep in mind this will make the
installation larger (see \code{?options} and search for \code{keep.source.pkgs}).
}
\examples{
insert_sort2 <- watch(insert_sort, c('x', 'i', 'j'))
res <- insert_sort2(runif(10))
dat <- simplify_data(attr(res, 'watch.data'))
code <- attr(res, 'watch.code')
}
\seealso{
\code{\link[=simplify_data]{simplify_data()}}, \code{vignette('watcher', package='watcher')}.
}
