

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
  
  # 'cumulative OR logic'
  # @param x \link[base]{logical} \link[base]{matrix}
  
  z <- x
  
  d <- dim(x)
  
  run <- x[1L, ]
  
  for (i in seq_len(d[1L])[-1L]) {
    z[i, ] <- 
      run <- 
      run | x[i, ] # wow!!
  }
  
  .rowSums(z, m = d[1L], n = d[2L])
  
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

