

#' @title `S4` Class \linkS4class{panel}
#' 
#' @slot m1,m0 \link[base]{logical} \link[base]{matrix}-es, true and false positives, respectively.
#' In other words, the variants tested positive in the positive and negative subjects (patients), respectively.
#' Rows are different variants.  Columns are different subjects.
#' The \link[base]{rownames} of `m0` and `m1` must be the same.
#' 
#' @slot id \link[base]{list} of \link[base]{character} \link[base]{vector}s
#' 
#' @slot ordered \link[base]{logical} scalar, whether this is an ordered \linkS4class{panel}
#' 
#' @slot label (optional) \link[base]{character} scalar, 
#' a human-friendly description of the \linkS4class{panel}
#' 
#' @slot consort (optional) \link[base]{data.frame} to 
#' create a \link[consort]{consort_plot}
#' 
#' @param m1,m0 see detailed explanations in Section **Slots**.
#' \itemize{
#' \item{If both `m1` and `m0` are missing, then random \link[base]{logical} \link[base]{matrix}-es
#' will be generated;}
#' \item{If one-and-only-one of `m1` and `m0` is missing, then the function [panel()] will \link[base]{stop}.}
#' }
#' 
#' 
#' @returns
#' The function [panel()] returns an R object of `S4` class \linkS4class{panel}.
#' 
#' 
#' @name panel
#' @aliases panel-class
#' @export
setClass(Class = 'panel', slots = c(
  m1 = 'matrix',
  m0 = 'matrix',
  id = 'list',
  ordered = 'logical',
  label = 'character',
  consort = 'data.frame'
), prototype = prototype(
  ordered = FALSE
))


#' @rdname panel
#' @export
panel <- function(m1 = zezulinski1, m0 = zezulinski0) {
  
  has1 <- !missing(m1)
  has0 <- !missing(m0)
  
  if (!has1 && !has0) {
    # random generation will be super tricky!!
    'Demo using the data from' |>
      col_red() |> style_bold() |>
      message()
    .zezulinski25() |> 
      format() |>
      message()
  } else if (xor(has1, has0)) {
    stop('one-and-only-one of `m1` and `m0` is missing!')
  }
  
  if (!is.matrix(m1) || !is.logical(m1)) stop('Variants tested positive in positive subjects `m1` must be logical-matrix') 
  if (!is.matrix(m0) || !is.logical(m0)) stop('Variants tested positive in negative subjects `m0` must be logical-matrix') 
  nr <- nrow(m1)
  if (nr != nrow(m0)) stop('`m0` and `m1` must have same number of rows')
  r <- rownames(m1)
  if (!identical(r, rownames(m0))) stop('`m0` and `m1` must have identical row-names')
  
  sr <- \(x) split.default(x, f = row(x)) # split-by-row
  match_id <- \(x, table) {
    id <- match(x = x, table = table)
    split.default(x = seq_along(id), f = id)
  }
  
  m <- cbind(m1, m0) # `m` could be huge
  
  uid <- !duplicated.matrix(m)
  
  id <- m[uid, , drop = FALSE] |>
    sr() |>
    match_id(x = sr(m), table = _) |>
    lapply(FUN = \(i) r[i])
  names(id) <- id |> 
    seq_along() |>
    format(justify = 'right') |>
    sprintf(fmt = 'Signature %s')
  
  m1o <- m1[uid, , drop = FALSE]
  m0o <- m0[uid, , drop = FALSE]
  rownames(m1o) <- rownames(m0o) <- names(id)
  
  new(
    Class = 'panel',
    m1 = m1o,
    m0 = m0o,
    id = id
  )
  
}


#' @title Show \linkS4class{panel}
#' 
#' @param object \linkS4class{panel}
#' 
#' @returns
#' The \link[methods]{show} method of \linkS4class{panel} class does not have a returned value.
#' 
#' @export
setMethod(f = show, signature = 'panel', definition = \(object) {
  
  if (length(object@label)) {
    object@label |>
      col_red() |>
      style_bold() |>
      cat(sep = '\n')
  }
  
  sprintf(
    fmt = 'Panel of %s Variant-Signatures from\n',
    object@m1 |> nrow() |> col_magenta()
  ) |> cat()
  sprintf(
    fmt = '%s %s subjects\n',
    object@m1 |> ncol() |> col_blue(),
    'positive' |> col_br_magenta()
  ) |> cat()
  sprintf(
    fmt = '%s %s subjects\n',
    object@m0 |> ncol() |> col_blue(),
    'negative' |> col_green()
  ) |> cat()
  
})








