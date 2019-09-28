# This works if wd is the website

# source('static/post/2019-01-11-reverse-polish-notation-parsing-in-r_files/rpn.R')

# We're going to loop through the body, and record start and end lines of
# the expressions.  Multi-line expressions are split into sub-expressions.
# This is lazy as it only works correctly if all expressions except
# "{" expressions like if, while, etc., are single line.

src_lines <- function(x, dat) {
  sub <- subset(dat, parent == x & token == 'expr')
  rows <- nrow(sub)
  res <- vector('list', rows)

  for(i in seq_len(rows))
    res[[i]] <-
      with(sub[i,], if(line2 - line1 > 0) src_lines(id, dat) else line1)
  res
}
# Track the monitored values, i, L, and udpate their display when they
# change.

enmonitor_one <- function(lang, line) {
  call(
    '{',
    call('<-', quote(.res), call("(", lang)),  # eval and temporarily store
    bquote(refresh_display(.(line))),          # update debug display
    quote(.res)                                # return temporary value
  )
}
enmonitor <- function(code, ln) {
  i <- j <- 1
  while(i <= length(code)) {
    while(
      is.name(code[[i]]) &&
      as.character(code[[i]]) %in% c("{", "while", "if")
    )
      i <- i + 1
    code[[i]] <- if(is.numeric(ln[[j]])) {
      enmonitor_one(code[[i]], ln[[j]])
    } else {
      enmonitor(code[[i]], ln[[j]])
    }
    i <- i + 1
    j <- j + 1
  }
  code
}
make_refresh_display <- function(src, idx.name, L.name, delay=delay) {
  L.old <- i.chr.init <- character()
  idx.old <- 0

  fun <- function(n) {
    if(!length(L.old)) writeLines(c('\n', src))
    L <- try(get(L.name, envir=parent.frame(), inherits=FALSE), silent=TRUE)
    idx <- try(get(idx.name, envir=parent.frame(), inherits=FALSE), silent=TRUE)

    if(inherits(L, 'try-error')) L <- list()
    if(inherits(idx, 'try-error')) idx <- 0

    L.chr <-
      paste0(" ", vapply(L, deparse, width.cutoff=500, character(1L)), " ")
    i.chr.pre <- i.chr <- if(!length(i.chr.init)) {
      i.chr.init <<- sprintf("[[%s]]:", seq_along(L))
      i.chr.init
    } else i.chr.init

    stopifnot(length(L.chr) <= length(src))

    del <- which(!L.old %in% L.chr)   # sloppy, only works for unique
    ins <- which(!L.chr %in% L.old)

    L.chr.pre <- L.old
    length(L.chr.pre) <- length(L.chr) <- length(i.chr) <- length(i.chr.pre) <-
      length(src) - 1L
    L.chr[is.na(L.chr)] <- ""
    L.chr.pre[is.na(L.chr.pre)] <- ""
    i.chr[is.na(i.chr)] <- ""
    i.chr.pre[is.na(i.chr.pre)] <- ""

    idx.ch <- isTRUE(idx != idx.old)
    flash <- length(del) > 0 || length(ins) > 0 || idx.ch

    if(idx.old) i.chr.pre[idx.old] <-
      sprintf("\033[7m%s\033[m", i.chr.pre[idx.old])
    if(idx) i.chr[idx] <-
      sprintf("\033[7%sm%s\033[m", if(!idx.ch) "" else ";93", i.chr[idx])
    src <- src.pre <- format(src)
    src.pre[n] <- sprintf("\033[7m%s\033[m", src[n])
    src[n] <- sprintf("\033[7%sm%s\033[m", if(!flash) "" else ";93", src[n])

    # Before, highlighting what's about to change

    L.chr.pre[del] <- sprintf("\033[93;7m%s\033[m", L.chr.pre[del])

    if(flash)
      redraw(
        paste0(
          pad(src.pre), "  ",
          pad(c('`L`', i.chr.pre)),
          pad(c('', L.chr.pre))
        ),
        delay
      )
    # After

    L.old <<- L.chr[nzchar(L.chr)]
    idx.old <<- idx

    if(length(ins)) L.chr[ins] <- sprintf("\033[93;7m%s\033[m", L.chr[ins])
    redraw(
      paste0(
        pad(src), "  ",
        pad(c('`L`', i.chr)),
        pad(c('', L.chr))
      ),
      delay
    )
  }
}
explain <- function(fun, delay=getOption('explain.delay')) {
  fun.name <- substitute(fun)
  stopifnot(is.name(fun.name))
  fun.name.chr <- as.character(fun.name)

  dat <- getParseData(fun)
  stopifnot(nrow(dat) > 0)

  symb.parent <- subset(dat, text==fun.name.chr & token == 'SYMBOL')$parent
  expr.parent <- subset(dat, id == symb.parent)$parent
  expr.func <- max(subset(dat, parent == expr.parent)$id)
  func.body.id <- max(subset(dat, parent == expr.func & token == 'expr')$id)

  src.ln <- src_lines(func.body.id, dat)
  src.ln <- rapply(
    src.ln, function(x) x - min(unlist(src.ln)) + 2, how='replace'
  )
  fun2 <- fun
  body(fun2) <- enmonitor(body(fun), src.ln)

  refresh_display <- make_refresh_display(
    deparse(fun, control='all'), 'i', 'L', delay=delay
  )
  environment(fun2) <- environment()
  fun2
}
## Helper funs

## pad with spaces accounting for ANSI CSI
nchar2 <- function(x) nchar(gsub('\033\\[[^m]*m', '', x))
pad <- function(x) {
  chars <- nchar2(x)
  paste0(x, strrep(" ", max(chars) - chars))
}
reset <- function(rows, cols) {
  whiteout <- rep(strrep(" ", cols), rows)
  cat(sprintf("\033[%dA\r", rows))
  writeLines(whiteout)
  cat(sprintf("\033[%dA\r", rows))
}
redraw <- function(out, delay) {
  reset(length(out) + 1, max(nchar2(out)))
  writeLines(out)
  delay_call(delay)
}
delay_call <- function(delay) {
  if(is.function(delay)) delay() else {
    if(!is.numeric(delay)) delay <- .75
    Sys.sleep(delay)
    cat('\n')
} }

