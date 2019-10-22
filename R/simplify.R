#' Simplify Watch Data
#'
#' Watch data is returned in a list with one element per step of the function
#' evaluation, which is awkward if we want to manipulate the data across steps.
#' This function will simplify variables that are either missing or always of
#' the same type according to the following rules:
#'
#' * Scalar values are turned into vectors and returned as a column of the
#'   ".scalar" data.frame member of the result.  Variables that are not defined
#'   at every step of the evaluation will be recorded as NA for those steps.
#' * Vectors are turned into data frames with columns `.id`, `.line` added.
#' * Matrices are turned into data frames with columns `.id`, `.line`, x, y,
#'   val, where `.id` is the step id and `.line` is the code line number.
#' * Data frames are rbinded and gain .id and .line variables, if those
#'   variables already exist they will be over-written.
#'
#' Variables that change types are just stored in their original format in the
#' list, except for scalars that change to vectors and vice versa, which are
#' treated as vectors.
#'
#' Not optimized for speed.
#'
#' Due to the use of the ".scalar", ".id", and ".line" symbols as part of the
#' simplification, the simplification will fail if the watched function contains
#' those symbols.  No effort is made to check for that condition.
#'
#' @seealso [watch()], [expand_text()]
#' @param dat list the "watch.data" attribute as produced by a [watch()]ed
#'   function.
#' @return list with elements '.scalar' which is a data frame of all
#'   the scalar variables with one row per step of the function evaluation, and
#'   additionally, one element per matrix/data.frame variable, and one list
#'   element for each of all the other variables.  Except for the ".scalar"
#'   element, each top-level element of the return value retains the name of the
#'   variable it corresponds to.
#' @export

simplify_data <- function(dat) {
  lines <- vapply(dat, `attr`, numeric(1L), 'line')
  vars <- unique(unlist(lapply(dat, names)))
  type <- setNames(character(length(vars)), vars)

  for(i in vars) {
    for(j in seq_along(dat)) {
      jval <- dat[[j]]
      type.tmp <- if(i %in% names(jval)) {
        if(is.matrix(jval[[i]]))
          'matrix'
        else if(length(jval[[i]]) == 1L && is.atomic(jval[[i]]))
          'scalar'
        else if(is.atomic(jval[[i]]))
          'vector'
        else if(is.data.frame(jval[[i]]))
          'data.frame'
        else 'list'
      } else ''

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
  res.scalar <- setNames(
    vector('list', sum(type == 'scalar') + 2L),
    c('.id', '.line', vars[type == 'scalar'])
  )
  for(i in vars[type == 'scalar']) {
    res.scalar[[i]] <-
      unlist(lapply(dat, function(x) if(!i %in% names(x)) NA else x[[i]]))
  }
  res.scalar[c('.id', '.line')] <- list(seq_along(lines), lines)
  res <- setNames(
    vector('list', length(vars[type != 'scalar']) + 1L),
    c('.scalar', vars[type != 'scalar'])
  )
  res[['.scalar']] <- as.data.frame(res.scalar)
  for(i in vars[type == 'matrix']) {
    tmp <- lapply(
      seq_along(dat), function(j) {
        x <- dat[[j]]
        data.frame(
          x=c(row(x[[i]])), y=c(col(x[[i]])), val=c(x[[i]]), .id=j
        )
      }
    )
    res[[i]] <- do.call(rbind, tmp)
  }
  for(i in vars[type == 'data.frame']) {
    tmp <- lapply(seq_along(dat), function(j) transform(dat[[j]][[i]], .id=j))
    res[[i]] <- do.call(rbind, tmp)
  }
  for(i in vars[type == 'vector']) {
    tmp <- lapply(
      seq_along(dat), function(j) data.frame(val=dat[[j]][[i]], .id=j)
    )
    res[[i]] <- do.call(rbind, tmp)
  }
  res
}
#' Expand Deparsed Code
#'
#' Aligns the "watch.code" data with the simplified "watch.data" by generating a
#' copy of "watch.code" for each step of the evaluation, and indicating which
#' line evaluation led to the corresponding state.
#'
#' Probably should be made part of [simplify_data()].  Not optimized.
#'
#' @param code the deparsed code code matching the watch data
#' @param data list watch data simplified with [simplify_data()]
#' @return data frame with the deparsed code repeated for each step of the
#'   function evaluation, with the line that was just evaluated marked with a
#'   TRUE in the `highlight` column.
#' @seealso [watch()], [simplify_data()]
#' @export

expand_text <- function(code, data) {
  id <- data[['.scalar']][['.id']]
  lines <- data[['.scalar']][['.line']]
  stopifnot(
    is.numeric(id), is.numeric(lines), length(id) == length(lines),
    is.character(code)
  )
  dat.lines <- do.call(
    rbind,
    lapply(
      id,
      function(i) {
        line <- lines[id == i]
        data.frame(
          y=(-seq_along(code) + line),
          y.raw=-seq_along(code) + length(code),
          .id=rep(i, length(code)),
          code=code,
          highlight=seq_along(code) == line
        )
  }) )
}
