

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







#' @title Convert a [panellist] into \link[flextable]{flextable}
#' 
#' @param x [panellist]
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @returns
#' The `S3` method [as_flextable.panellist()] returns a \link[flextable]{flextable}.
#'
#' @note
#' The `S3` method [as_flextable.panellist()] may be eventually deprecated.
#' 
#' 
#' @importFrom scales pal_hue
#' @export as_flextable.panellist
#' @export
as_flextable.panellist <- function(x, ...) {
  
  nx <- length(x)
  
  vs <- x |> 
    lapply(FUN = slot, name = 'id')
  
  v0 <- vs |>
    unname() |> # just to be sure
    unlist(recursive = FALSE, use.names = TRUE)
  # unique.default() drops names!
  v <- v0[!duplicated(v0)] # do NOT sort !!!!
  
  diff_v <- vs |>
    seq_along() |>
    lapply(FUN = \(i) {
      vs[seq_len(i-1L)] |>
        lapply(FUN = names) |> 
        unlist(use.names = FALSE) |>
        setdiff(x = names(vs[[i]]), y = _)
    })
  
  # amount of incremental addition
  # to determine position of ?flextable::hline
  nv <- diff_v |>
    lengths(use.names = FALSE) 
  
  nvr <- v |> 
    lengths(use.names = FALSE) # multiple variants per signature
  
  m1. <- mapply(
    FUN = \(x, v) {
      x@m1[v, , drop = FALSE]
    }, 
    x = x, v = diff_v, 
    SIMPLIFY = FALSE
  ) |>
    do.call(what = rbind, args = _)
  
  m0. <- mapply(
    FUN = \(x, v) {
      x@m0[v, , drop = FALSE]
    }, 
    x = x, v = diff_v, 
    SIMPLIFY = FALSE
  ) |>
    do.call(what = rbind, args = _)
  
  d <- data.frame(
    'Variant-Signature' = v |> names(),
    'True(+)' = sprintf(fmt = '%d/%d', rowSums(m1.), ncol(m1.)),
    'False(+)' = sprintf(fmt = '%d/%d', rowSums(m0.), ncol(m0.)),
    'Variants in Signature' = v |>
      vapply(FUN = paste, collapse = '\n', FUN.VALUE = NA_character_),
    check.names = FALSE
  )
  
  tmp <- x |>
    setNames(
      nm = x |> 
        vapply(FUN = \(i) {
          sprintf(fmt = '%s\nsize=%d', i@label, nrow(i@m1))
        }, FUN.VALUE = '')
    ) |>
    mapply(
      FUN = \(x, vs) {
        ifelse(
          test = names(v) %in% names(vs), 
          yes = x@label,
          no = ''
        )
      }, x = _, vs = vs, SIMPLIFY = FALSE
    ) |>
    as.data.frame.list(check.names = FALSE)
  
  data.frame(
    d, tmp,
    check.names = FALSE
  ) |> 
    flextable() |>
    autofit() |>
    hline(i = cumsum(nv)[-length(nv)]) |>
    color(j = length(d) + seq_len(nx), color = pal_hue()(n = nx), part = 'all') |>
    highlight(i = (nvr > 1L), j = length(d), color = 'lightyellow') |>
    align(align = 'right', part = 'all') |>
    add_header_row(
      values = c('Variant-Signature', 'Individual', 'Variants in Signature', 'Ordered Panel'), 
      colwidths = c(1L, 2L, 1L, length(tmp)), 
      top = TRUE) |> 
    merge_v(part = 'header') |>
    align(align = 'center', part = 'all') |>
    #bold(i = 1L, part = 'header') |> 
    bold(part = 'header') |> 
    color(i = 1L, color = 'black', part = 'header')
  
}




