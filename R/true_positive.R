

#' @title Number of True & False Positives, per variant
#' 
#' @param x a \linkS4class{panel}
#' 
#' @returns
#' Functions [sum1()] and [sum0()] return 
#' a \link[base]{integer} \link[base]{vector}.
#' 
#' Function [cumsum1()] and [cumsum0()] return
#' a (not strictly) increasing \link[base]{integer} \link[base]{vector}.
#' 
#' @keywords internal
#' @name sum1
#' @export
sum1 <- function(x) {
  x@m1 |>
    rowSums() # number of true positives, per variant
}

#' @rdname sum1
#' @export
sum0 <- function(x) {
  x@m0 |> 
    rowSums() # number of false positives, per variant
}


#' @importFrom matrixStats colAnys
.total_positive <- \(x) {
  # `x` is a \link[base]{logical} \link[base]{matrix}
  x |> 
    colAnys() |>
    sum()
}


.cum_positive <- \(x) {
  # `x` is a \link[base]{logical} \link[base]{matrix}
  x |>
    nrow() |>
    seq_len() |> 
    vapply(FUN = \(i) {
      x[seq_len(i), , drop = FALSE] |>
        .total_positive()
    }, FUN.VALUE = NA_integer_)
}

#' @rdname sum1
#' @export
cumsum1 <- function(x) {
  x@m1 |> 
    .cum_positive()
}

#' @rdname sum1
#' @export
cumsum0 <- function(x) {
  x@m0 |>
    .cum_positive()
}

