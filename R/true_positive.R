

#' @title Number of True & False Positives
#' 
#' @param x \linkS4class{panel}
#' 
#' @returns
#' The functions [sum1()] and [sum0()] return 
#' a \link[base]{integer} \link[base]{vector}.
#' 
#' The functions [cumsum1()] and [cumsum0()] return
#' a non-decreasing \link[base]{integer} \link[base]{vector}.
#' 
#' @name sum1
#' @export
sum1 <- function(x) rowSums(x@m1) # number of true positives, per variant

#' @rdname sum1
#' @export
sum0 <- function(x) rowSums(x@m0) # number of false positives, per variant

#' @rdname sum1
#' @export
cumsum1 <- function(x) cumOR(x@m1)

#' @rdname sum1
#' @export
cumsum0 <- function(x) cumOR(x@m0)





# 'cumulative OR logic'
# @param x \link[base]{logical} \link[base]{matrix}
cumOR <- \(x) {

  d <- dim(x)
  if (d[1L] == 0L) return(integer()) # exception handling
  
  # slower with large-nrow matrix!!
  # z <- x
  # run <- x[1L, ]
  # for (i in seq_len(d[1L])[-1L]) {
  #   z[i, ] <- run <- (run | x[i, ])
  # }
  # .rowSums(z, m = d[1L], n = d[2L])
  # end of slower with large-nrow matrix!!
  
  x |>
    asplit(MARGIN = 1, drop = TRUE) |>
    Reduce(f = `|`, x = _, accumulate = TRUE) |>
    #vapply(FUN = sum, FUN.VALUE = NA_integer_) # slightly slower
    do.call(what = rbind, args = _) |>
    .rowSums(m = d[1L], n = d[2L])

}






