
# Compute Start Line of Each Top-Level Expression
#
# We're going to loop through the body, and record start lines of
# the expressions.  Multi-line expressions are split into sub-expressions.
# This is lazy as it only works correctly if all expressions except
# "{" expressions like if, while, etc., are single line.
#
# In order to do this properly we would have to explicitly detect all "{"
# expressions explicitly to only recurse on those, but doing so requires
# conditionally peeking down two levels looking for a "{" (check if
# "while/repeat/for", and if so, look for "expr", and look at child to see if
# it starts with "{").
#
# @param dat parse data as produced by `getParseData`
# @param x an id from the parseData `dat` that we wish to examine the children
#   of.
# @return a nested list that mimics the structure of the language object that
#   produced the parse data

src_lines <- function(x, dat) {
  if(any(dat[['token']] == 'exprlist'))
    stop('exprlist not supported (expressions with ";" in them')

  x.dat <- dat[dat[['id']] == x, ,drop=FALSE]
  if(nrow(x.dat) != 1L || !identical(x.dat[['token']], 'expr'))
    stop("Can only compute `src_lines` on 'expr' tokens")

  sub <- subset(dat, parent == x)
  if(!nrow(sub)) stop("no children for ", x)
  sub.start <- sub[['token']][1L]
  ctrl.if <- c('WHILE', 'IF')
  if(sub.start %in% c(ctrl.if, "'{'", 'FOR', 'REPEAT')) {
    sub.expr <- subset(sub, token == 'expr')
    if(sub.start %in% ctrl.if) sub.expr <- sub.expr[-1L,,drop=FALSE]
    rows <- nrow(sub.expr)
    res <- vector('list', rows)
    for(i in seq_len(rows)) res[[i]] <- src_lines(sub.expr[['id']][[i]], dat)
    res
  } else {
    x.dat[['line1']]
  }
}
watch.data <- new.env()

#' Manage Watch Data
#'
#' Functions to collect and process watch data.
#'
#' @export

watch_data <- function() {
  watch.data[['data']]
}
#' @rdname watch_data
#' @export

watch_init <- function(vars) {
  watch.data[['data']] <- list()
  watch.data[['vars']] <- vars
}
#' @rdname watch_data
#' @export

capture_data <- function(env, line) {
  dat <- if(length(watch.data[['vars']])) {
    env.vars <- ls(envir=env)
    mget(intersect(watch.data[['vars']], env.vars), envir=env, inherits=FALSE)
  } else as.list(env)

  attr(dat, 'line') <- line
  watch.data[['data']] <- append(watch.data[['data']], list(dat))
  invisible(NULL)
}
#' Simplify Watch Data
#'
#' Watch data is returned in a list with one element per step of the function
#' evaluation, which is awkward if we want to manipulate the data across steps.
#' This function will simplify variables that are either missing or always of
#' the same type according to the following rules:
#'
#' * Scalar values are turned into vectors, NA entries when the variable is not
#'   present (e.g. when function begins evaluation in variable is not defined
#'   yet)
#' * Vectors are turned into data frames with columns .id, .line added.
#' * Matrices are turned into data frames with columns .id, .line, x, y, val,
#'   where .id is the step id and .line is the code line number.
#' * Data frames are rbinded and gain .id and .line variables, if those
#'   variables already exist they will be over-written.
#'
#' Variables that change types are just stored in their original format in the
#' list, except for scalars that change to vectors and vice versa, which are
#' treated as vectors.
#'
#' Not optimized for speed.
#'
#' @param dat list data produced by a [watch()]ed function.
#' @return list with elements 'scalar', 'matrix', 'data.frame', and 'other',
#'   each of those with sub-elements, each of those of the simplified type as in
#'   the description.
#' @export

simplify_data <- function(dat) {
  lines <- vapply(dat, `attr`, numeric(1L), 'line')
  vars <- unique(unlist(lapply(dat, names)))
  type <- setNames(character(length(vars)), vars)

  for(i in vars) {
    for(j in seq_along(dat)) {
      jval <- dat[[j]]
      if(i %in% names(jval)) {
        type.tmp <- if(is.matrix(jval[[i]]))
          'matrix'
        else if(length(jval[[i]]) == 1L && is.atomic(jval[[i]]))
          'scalar'
        else if(is.atomic(jval[[i]]))
          'vector'
        else if(is.data.frame(jval[[i]]))
          'data.frame'
        else 'list'
      }
      if (type[[i]] == '') {
        type[[i]] <- type.tmp
      } else if (type[[i]] != type.tmp) {
        if(all(c(type[[i]], type.tmp) %in% c('scalar', 'vector'))) {
          type[[i]] <- 'vector'
        } else {
          type[[i]] <- 'list'
          break
        }
      }
  } }
  stopifnot(all(nzchar(type)))
  res <- setNames(vector('list', length(vars) + 1L), c('.line', vars))
  res[['.line']] <- lines
  for(i in vars[type == 'scalar']) {
    res[[i]] <-
      unlist(lapply(dat, function(x) if(!i %in% names(x)) NA else x[[i]]))
  }
  for(i in vars[type == 'matrix']) {
    tmp <- lapply(
      seq_along(dat), function(j) {
        x <- dat[[j]]
        data.frame(
          x=c(row(x[[i]])), y=c(row(x[[i]])), val=c(x[[i]]), .id=j
        )
      }
    )
    res[[i]] <- do.call(rbind, tmp)
  }
  for(i in vars[type == 'data.frame']) {
    tmp <- lapply(seq_along(dat), function(j) transform(dat[[j]], .id=j))
    res[[i]] <- do.call(rbind, tmp)
  }
  for(i in vars[type == 'vector']) {
    tmp <- lapply(seq_along(dat), function(j) data.frame(dat[[j]], .id=j))
    res[[i]] <- do.call(rbind, tmp)
  }
  res
}

enmonitor_one <- function(lang, line) {
  call(
    '{',
    call('<-', quote(.res), call("(", lang)),
    bquote(watcher::capture_data(environment(), .(line))),
    quote(.res)
  )
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
    if(is.name(code[[i]])) {
      # control structures need to skip their control portion
      symb <- as.character(code[[i]])
      i <- i +
        1 * (symb %in% c('{', 'repeat')) +
        2 * (symb %in% c('if', 'while')) +
        3 * (symb == 'for')
    }
    code[[i]] <- if(is.numeric(ln[[j]])) {
      # top level statement, monitor the element
      enmonitor_one(code[[i]], ln[[j]])
    } else {
      # not top level, so recurse
      enmonitor(code[[i]], ln[[j]])
    }
    i <- i + 1
    j <- j + 1
  }
  code
}
#' Modify a function to be watched
#'
#' @export

watch <- function(fun, vars=character()) {
  fun.name <- substitute(fun)
  stopifnot(is.name(fun.name))
  fun.name.chr <- as.character(fun.name)

  dat <- getParseData(fun)
  stopifnot(nrow(dat) > 0)

  # find function body in parse data

  symb.parent <- subset(dat, text==fun.name.chr & token == 'SYMBOL')$parent
  expr.parent <- subset(dat, id == symb.parent)$parent
  expr.func <- max(subset(dat, parent == expr.parent)$id)
  func.body.id <- max(subset(dat, parent == expr.func & token == 'expr')$id)

  src.ln <- src_lines(func.body.id, dat)
  src.ln <- rapply(
    src.ln, function(x) x - min(unlist(src.ln)) + 2, how='replace'
  )
  fun2 <- fun
  fun.body.raw <- enmonitor(body(fun), src.ln)
  fun.body <- quote({
    watcher::watch_init(vars)
    res <- NULL
    attr(res, 'watch.data') <- watcher::watch_data()
    res
  })
  fun.body[[3L]][[3L]] <- fun.body.raw
  body(fun2) <- fun.body
  environment(fun2) <- environment()   # is this right?  Doesn't seem so
  fun2
}
## Helper funs

## pad with spaces accounting for ANSI CSI
nchar2 <- function(x) nchar(gsub('\033\\[[^m]*m', '', x))
pad <- function(x) {
  chars <- nchar2(x)
  paste0(x, strrep(" ", max(chars) - chars))
}
