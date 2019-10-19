
#' Implementation of Insertion Sort
#'
#' For use in examples, taken from
#' [wikipedia](#https://en.wikipedia.org/wiki/Insertion_sort)
#'
#' @export
#' @param x a vector to sort
#' @return a sorted vector

insert_sort <- function(x) {
  i <- 2
  while(i <= length(x)) {
    j <- i
    while(j > 1 && x[j - 1] > x[j]) {
      j <- j - 1
      x[j + 0:1] <- x[j + 1:0]
    }
    i <- i + 1
  }
  x
}
