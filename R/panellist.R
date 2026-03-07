

#' @title `panellist`
#' 
#' @description
#' To combine multiple \linkS4class{panel}s.
#' 
#' @param ... one or more \linkS4class{panel}s
#' 
#' @returns
#' The function [panellist()] returns an `S3` object of `panellist`,
#' which inherits from the classes \link[stats]{listof} and \link[base]{list}.
#' 
#' 
#' @export
panellist <- function(...) {
  
  z <- list(...)
  
  if (!all(vapply(z, FUN = inherits, what = 'panel', FUN.VALUE = TRUE))) {
    stop('All input must be `panel` objects')
  }
    
  class(z) <- c('panellist', 'listof', class(z)) |>
    unique.default()
  # to make use of
  # base::print.listof
  
  return(z)
  
}







#' @title Visualize [panellist] using Package \CRANpkg{ggplot2}
#' 
#' @param object [panellist]
#' 
#' @param which \link[base]{character} scalar, 
#' `'oc'` (default value) or (faux) `'roc'`
#' 
#' @param ... additional parameters, currently no use
#' 
#' @returns
#' The `S3` method [autoplot.panellist()] returns a \link[ggplot2]{ggplot} object.
#' 
#' @name autoplot.panellist
#' @export autoplot.panellist
#' @export
autoplot.panellist <- function(object, ...) {
  ggplot() + 
    autolayer.panellist(object, ...)
}

#' @rdname autoplot.panellist
#' @importFrom scales label_percent
#' @export autolayer.panellist
#' @export
autolayer.panellist <- function(
    object, 
    which = c('oc', 'roc'),
    ...
) {
  
  ord. <- object |>
    vapply(FUN = slot, name = 'ordered', FUN.VALUE = NA) |>
    all()
  if (!ord.) stop('all `panel`s must be ordered')
  
  n1 <- object[[1L]]@m1 |>
    ncol()
  n0 <- object[[1L]]@m0 |>
    ncol()
  
  cumsum1. <- object |> 
    lapply(FUN = cumsum1)
  
  which <- match.arg(which)
  
  .x <- switch(which, oc = {
    cumsum1. |> 
      lapply(FUN = seq_along) |> 
      unlist()
  }, roc = {
    cumsum0.. <- object |> 
      lapply(FUN = cumsum0) |>
      unlist()
    (cumsum0.. / n0)
  })
  sens <- (cumsum1. |> unlist()) / n1
  .group <- object |> 
    seq_along() |> 
    rep(times = lengths(cumsum1.)) |>
    factor()
  .label <- object |> 
    vapply(FUN = \(i) {
      inm <- i@label
      if (length(inm)) return(inm)
      i@m1 |> nrow() |> as.character()
    }, FUN.VALUE = '')
  
  mp <- aes(x = .x, y = sens, color = .group)
  
  out <- list(
    geom_point(mapping = mp),
    geom_path(mapping = mp, linewidth = .3),
    scale_y_continuous(name = 'Sub-Panel Sensitivity', labels = label_percent(), limits = c(0, 1)),
    switch(which, oc = {
      scale_x_continuous(name = 'Number of Signatures per Sub-Panel')
    }, roc = {
      scale_x_continuous(name = 'Sub-Panel Non-Specificity', labels = label_percent(), limits = c(0, 1))
    }),
    scale_color_discrete(name = 'Ordered Sub-Panels', labels = .label)
  )

  switch(which, roc = {
    return(c(
      out,
      list(
        geom_abline(slope = 1, intercept = 0, color = 'grey80', linetype = 2L),
        coord_equal()
      )
    ))
  }, oc = return(out))
  
}










