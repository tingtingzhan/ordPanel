

#' @title CONSORT-Like Flow-Chart of Ordered \linkS4class{panel}
#' 
#' @description
#' To create a CONSORT-Like flow-chart of an ordered \linkS4class{panel}.
#' 
#' @param x an ordered \linkS4class{panel}
#' 
#' @param ... additional parameters of the function \link[consort]{consort_plot},
#' except for `data`, `orders` and `side_box`
#' 
#' @returns 
#' The function [plot.panel()] returns an R object of class `'consort'`.
#' 
#' @importFrom consort consort_plot
#' @export plot.panel
#' @export
plot.panel <- function(x, ...) {
  
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



#' @title CONSORT-Like Flow-Chart of Ordered [panellist]
#' 
#' @description
#' To create a CONSORT-Like flow-chart of an ordered [panellist].
#' 
#' @param x an ordered [panellist]
#' 
#' @param ... additional parameters for the function \link[patchwork]{wrap_plots}, 
#' **not** for the function [plot.panel()]
#' 
#' @returns 
#' The function [plot.panellist()] returns a \link[patchwork]{patchwork}.
#' 
#' @importFrom consort build_grid
#' @export plot.panellist
#' @export
plot.panellist <- function(x, ...) {
  
  x |> 
    lapply(FUN = \(i) {
      i |> 
        plot.panel() |> 
        build_grid() |>
        wrap_elements()
    }) |>
    c(list(...)) |> 
    do.call(what = 'wrap_plots', args = _)

}
  
  
  
  
