
# Compute Start Line of Each Top-Level Expression
#
# We're going to loop through the body, and record start lines of
# the expressions.  Multi-line expressions are split into sub-expressions.
#
# Definitely not fully robust, in particular to exprlist tokens (i.e. those
# produced by `;` delimited expressions), which kinda work although line numbers
# are missreported and other issues.
#
# @param dat parse data as produced by `getParseData`
# @param x an id from the parseData `dat` that we wish to examine the children
#   of.
# @return a nested list that mimics the structure of the language object that
#   produced the parse data

src_lines <- function(x, dat) {
  x.dat <- dat[dat[['id']] == x, ,drop=FALSE]
  sub <- subset(dat, parent == x)
  if(!nrow(sub)) stop("no children for ", x)
  sub.start <- sub[['token']][1L]
  ctrl.if <- c('WHILE', 'IF')
  sub.expr <- subset(sub, token == 'expr')
  if(sub.start %in% c(ctrl.if, "'{'", 'FOR', 'REPEAT') & nrow(sub.expr) > 0) {
    res <- vector('list', nrow(sub.expr))
    for(i in seq_len(nrow(sub.expr))) {
      res[[i]] <- if(sub.start %in% ctrl.if && i == 1) {
        sub.expr[i, 'line1']
      } else {
        src_lines(sub.expr[['id']][[i]], dat)
      }
    }
    res
  } else if (identical(sub.start, "'{'") && nrow(sub.expr) > 2) {
    sub.start[2, 'line1']
  } else {
    x.dat[['line1']]
  }
}
watch.data <- new.env()

## Manage Watch Data
##
## Functions to collect and process watch data.

watch_data <- function() {
  watch.data[['data']]
}
watch_init <- function(vars) {
  watch.data[['data']] <- list()
  watch.data[['vars']] <- vars
}
capture_data <- function(env, line) {
  dat <- if(length(watch.data[['vars']])) {
    env.vars <- ls(envir=env)
    mget(intersect(watch.data[['vars']], env.vars), envir=env, inherits=FALSE)
  } else as.list(env)

  attr(dat, 'line') <- line
  watch.data[['data']] <- append(watch.data[['data']], list(dat))
  invisible(NULL)
}
# Important for `code` and `ln` to be aligned, so we need `src_lines` to
# correctly determine which elements are nesting vs not, and `enmonitor` to
# correctly skip the parts that are not part of the nesting.
#
# Most annoying thing here is the difference between `for` and `if/while`, and
# to a lesser extent the differnce between if and if/else (else if is just a
# nested if/else).
#
# Here we compare language objects to the corresponding parse data:
#
# for-language (for(i in x) body):
#
# 1. `for`
# 2. `i`                    # skip
# 3. `x`                    # skip
# 4. `body`
#
# for-dat, subset to FOR/forcond/expr
#
# 1. FOR
# 2. forcond "(i in x)"     # skip
# 3. body
#
# ifelse-language (if(a) b else c)
#
# 1. `if`
# 2. `a`                    # skip
# 3. `b`
# 4. `c`                    # optional?
#
# ifelse-dat, subset to IF/expr
#
# 1. IF
# 2. a                      # skip
# 3. b
# 4. c                      # optional?
#
# while is like ifelse without the else, repeat probably similar.
#
# We need logic to skip the right elements when computing start lines, and
# also when "enmonitoring" them.

# Big difference between '{' and the control structures, as for this one
# the call itself contains everything, whereas for the others??  I guess it's
# the same, except you have 'if' instead of '{', and then you have to skip stuff

enmonitor <- function(code, ln) {
  i <- j <- 1
  while(i <= length(code)) {
    if(is.name(code[[i]]) && (length(ln) > 1L || is.list(ln))) {
      # control structures need to skip their control portion, though
      # note we don't enter ehere if '{' only contains exprlist
      symb <- as.character(code[[i]])
      i <- i +
        1 * (symb %in% c('{', 'repeat')) +
        1 * (symb %in% c('if', 'while')) +
        3 * (symb == 'for')
    }
    if(is.numeric(ln[[j]])) {
      # top level statement, monitor the element
      if(identical(as.character(code[[i]]), '{')) {
        # special case of `{` containing exprlist (hack alert)
        code <- enmonitor_one(code, ln[[j]])
        break
      } else {
        code[[i]] <- enmonitor_one(code[[i]], ln[[j]])
      }
    } else {
      # not top level, so recurse
      code[[i]] <- enmonitor(code[[i]], ln[[j]])
    }
    i <- i + 1
    j <- j + 1
  }
  code
}
enmonitor_one <- function(lang, line) {
  call(
    '{',
    call('<-', quote(.res), call("(", lang)),
    bquote(watcher:::capture_data(environment(), .(line))),
    quote(.res)
  )
}

#' Instrument a Function for Variable Watching
#'
#' The input function will be modified such that the state of the function
#' environment is captured after each top-level statement is evaluated.
#' Top-level statements are, roughly speaking, distinct R expressions that are
#' visible directly in the source of the R function.
#'
#' For each top-level step in the evaluation of a watched function a list
#' element is added to the "watch.data" attribute of the result.  The list will
#' contain a copy of all the variables in the function environment right after
#' that step is evaluated, or of the subset of them specified by the `vars`
#' parameter.  Additionally, the list will contain a "line" attribute that maps
#' to the start line of the top-level expression that was last evaluated.  This
#' line number maps to the deparsed function attached to the result as the
#' "watch.code" attribute.  The line numbers may or may not match to other
#' deparsings of the function, so if you intend on using the line numbers be
#' sure to do so in relation to the "watch.code" version of the function.
#'
#' Instrumented function semantics should be the same as the un-instrumented
#' version, except for the attributes attached to the return value, and for the
#' use of the `.res` temporary variable.  If a function also uses the `.res`
#' symbol the instrumentation will interfere with the original semantics.
#'
#' @note if you are watching a function from a package you might want to install
#'   the package with `Sys.setenv('R_KEEP_PKG_SOURCE'='yes')` so that the line
#'   numbers match the original source, although keep in mind this will make the
#'   installation larger (see `?options` and search for `keep.source.pkgs`).
#' @param fun a function to watch
#' @param vars character a vector of names of variables to record
#' @return an instrumented version of `fun`.  When this instrumented function is
#'   run it will add attributes "watch.data" and "watch.code" to the result.
#'   See the description for details about return data format of the
#'   instrumented function.
#'
#' @seealso [simplify_data()], `vignette('watcher', package='watcher')`.
#' @export
#' @examples
#' insert_sort2 <- watch(insert_sort, c('x', 'i', 'j'))
#' res <- insert_sort2(runif(10))
#' dat <- simplify_data(attr(res, 'watch.data'))
#' code <- attr(res, 'watch.code')

watch <- function(fun, vars=character()) {
  stopifnot(is.function(fun), is.character(vars), !anyNA(vars))
  fun.name <- substitute(fun)
  stopifnot(is.name(fun.name))
  fun.name.chr <- as.character(fun.name)

  # find function body in parse data.  Pkg funs by default don't keep parse
  # data, but there are global options that change behavior (search for "keep"
  # in `?options`.

  code <- deparse(fun, control='all')
  dat <- getParseData(fun)
  if(is.null(dat)) {
    code[[1]] <- paste0(fun.name.chr, " <- ", code[[1]])
    old.opt <- options(keep.parse.data=TRUE)
    on.exit(options(old.opt))
    dat <- getParseData(parse(text=code, keep.source=TRUE))
  }
  if(nrow(dat) < 1) stop("Parse data missing.")

  symb.parent <-
    tail(subset(dat, text==fun.name.chr & token == 'SYMBOL'), 1)$parent
  if(length(symb.parent) != 1)
    stop("Failed finding parse data for function ", fun.name.chr)

  expr.parent <- subset(dat, id == symb.parent)$parent
  expr.func <- max(subset(dat, parent == expr.parent)$id)
  expr.func.start <- subset(dat, id == expr.func)[['line1']]
  func.body.id <- max(subset(dat, parent == expr.func & token == 'expr')$id)

  src.ln <- src_lines(func.body.id, dat)
  src.ln <- if(is.list(src.ln)) {
    rapply(src.ln, function(x) x - expr.func.start + 1L, how='replace')
  } else {
    list(src.ln - expr.func.start + 1L)
  }
  fun2 <- fun
  fun.body.raw <- enmonitor(body(fun), src.ln)
  fun.body <- bquote({
    watcher:::watch_init(.(vars))
    res <- NULL
    attr(res, 'watch.data') <- watcher:::watch_data()
    attr(res, 'watch.code') <- .(code)
    res
  })
  fun.body[[3L]][[3L]] <- fun.body.raw
  body(fun2) <- fun.body
  # environment(fun2) <- environment()   # is this right?  Doesn't seem so
  fun2
}
