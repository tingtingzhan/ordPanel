

#' @title Number of True & False Positives, per variant
#' 
#' @param x a \linkS4class{panel}
#' 
#' @returns
#' Functions [true_positive()] and [false_positive()] return 
#' a \link[base]{integer} \link[base]{vector}.
#' 
#' Function [cum_true_positive()] and [cum_false_positive()] return
#' a (not strictly) increasing \link[base]{integer} \link[base]{vector}.
#' 
#' @keywords internal
#' @name true_positive
#' @export
true_positive <- function(x) {
  x@m1 |>
    rowSums() # number of true positives, per variant
}

#' @rdname true_positive
#' @export
false_positive <- function(x) {
  x@m0 |> 
    rowSums() # number of false positives, per variant
}


#' @importFrom matrixStats colAnys
.total_positive <- function(x) {
  # `x` is a \link[base]{logical} \link[base]{matrix}
  x |> 
    colAnys() |>
    sum()
}


.cum_positive <- function(x) {
  # `x` is a \link[base]{logical} \link[base]{matrix}
  x |>
    nrow() |>
    seq_len() |> 
    vapply(FUN = \(i) {
      x[seq_len(i), , drop = FALSE] |>
        .total_positive()
    }, FUN.VALUE = NA_integer_)
}

#' @rdname true_positive
#' @export
cum_true_positive <- function(x) {
  x@m1 |> 
    .cum_positive()
}

#' @rdname true_positive
#' @export
cum_false_positive <- function(x) {
  x@m0 |>
    .cum_positive()
}

