

#' @title [consort.panel()]
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param ... additional parameters of function \link[consort]{consort_plot},
#' except for `data`, `orders` and `side_box`
#' 
#' @keywords internal
#' @importFrom consort consort_plot
#' @export
consort.panel <- function(x, ...) {
  
  cst <- x@consort
  nc <- length(cst)
  
  ord <- rep('Signatures', times = nc * 2 + 1L)
  names(ord) <- rep('signature', times = length(ord))
  side_id <- seq_len(nc) * 2
  ord[side_id] <- cst |> 
    vapply(FUN = attr, which = 'label', exact = TRUE, FUN.VALUE = '')
  names(ord)[side_id] <- cst |> 
    names()
  
  data.frame(
    signature = cst |> nrow() |> seq_len(),
    cst
  ) |>
    consort_plot(data = _, orders = ord, side_box = names(cst), ...)
    
} 



#' @title [consort.panellist()]
#' 
#' @param x a [panellist]
#' 
#' @param ... additional parameters for function \link[patchwork]{wrap_plots}, 
#' **not** for function [consort.panel()]
#' 
#' @keywords internal
#' @importFrom consort build_grid
#' @export
consort.panellist <- function(x, ...) {
  
  x |> 
    lapply(FUN = \(i) {
      i |> 
        consort.panel() |> 
        build_grid() |>
        wrap_elements()
    }) |>
    c(list(...)) |> 
    do.call(what = 'wrap_plots', args = _)

}
  
  
  
  
