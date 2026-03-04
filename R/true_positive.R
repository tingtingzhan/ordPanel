

#' @title Number of True & False Positives, per variant
#' 
#' @param x \linkS4class{panel}
#' 
#' @returns
#' The functions [sum1()] and [sum0()] return 
#' a \link[base]{integer} \link[base]{vector}.
#' 
#' The functions [cumsum1()] and [cumsum0()] return
#' a **strictly** increasing \link[base]{integer} \link[base]{vector}.
#' 
#' 
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



cumOR <- \(x) {
  
  # Claude !!!
  # 'cumulative OR logic'
  # @param x \link[base]{logical} \link[base]{matrix}
  
  nr <- nrow(x)
  out <- integer(length = nr)
  
  seen <- x[1L, ]
  out[1L] <- sum(seen)
  
  for (i in seq_len(nr)[-1L]) {
    seen <- seen | x[i, ]
    out[i] <- sum(seen)
  }
  
  return(out)
  
}



#' @rdname sum1
#' @export
cumsum1 <- function(x) {
  x@m1 |> 
    cumOR()
}

#' @rdname sum1
#' @export
cumsum0 <- function(x) {
  x@m0 |>
    cumOR()
}

