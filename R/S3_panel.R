


#' @title Extract Rows of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param i \link[base]{logical} \link[base]{vector}, row indices
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function ?? returns a \linkS4class{panel}.
#' 
#' @export
`[.panel` <- function(x, i, ...) {
  new(
    Class = 'panel',
    m1 = x@m1[i, , drop = FALSE],
    m0 = x@m0[i, , drop = FALSE],
    id = x@id[i],
    label = x@label#,
    #consort = x@consort[i, , drop = FALSE] # do NOT do anything here!!! 
  )
}







#' @title Select a \link[base]{subset} of \linkS4class{panel}
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param subset a \link[base]{language} object
#' 
#' @param append.label \link[base]{logical} scalar, whether to 
#' append the criterion `subset` to `x@label`, default `FALSE`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @export subset.panel
#' @export
subset.panel <- function(x, subset, append.label = FALSE, ...) {
  
  e <- substitute(subset)
  e. <- e
  
  .symbol <- e[[1L]] |> 
    deparse1() |> 
    switch(EXPR = _, '<=' = '\u2264', '>=' = '\u2265', '<' = '<', '>' = '>')
  
  # invert symbol
  .inv_symbol <- e[[1L]] |> 
    deparse1() |> 
    switch(EXPR = _, '<=' = '>', '>=' = '<', '<' = '\u2265', '>' = '\u2264')
  
  e[[2L]] |>
    deparse1() |>
    switch(EXPR = _, sum1 = {
      e.[[2L]] <- call(name = 'sum1', quote(x))
      id <- eval(e.)
      .crit <- 'True(+)'
      .labs <- sprintf(fmt = 'True(+) %s%d/%d', c(.symbol, .inv_symbol), e.[[3L]], x@m1 |> ncol())
    }, sum0 = {
      e.[[2L]] <- call(name = 'sum0', quote(x))
      id <- eval(e.)
      .crit <- 'False(+)'
      .labs <- sprintf(fmt = 'False(+) %s%d/%d', c(.symbol, .inv_symbol), e.[[3L]], x@m0 |> ncol())
    }, cumsum0 = {
      e.[[2L]] <- call(name = 'cumsum0', quote(x))
      id <- eval(e.)
      .crit <- 'panelFalse(+)'
      .labs <- sprintf(fmt = 'panelFalse(+) %s%d/%d', c(.symbol, .inv_symbol), e.[[3L]], x@m0 |> ncol())
    }, 'diff(cumsum1)' = {
      e.[[2L]] <- call(name = 'diff', call(name = 'cumsum1', quote(x))) # cannot use native pipe!
      id <- c(1L, which(eval(e.)) + 1L)
      .crit <- 'diffTrue(+)'
      .labs <- sprintf(fmt = 'diffTrue(+) %s%d/%d', c(.symbol, .inv_symbol), e.[[3L]], x@m1 |> ncol())
    })

  ret <- x[id, ] # `[.panel`
  
  if (length(x@consort)) {
    
    # previously eligible
    id0 <- x@consort |>
      lapply(FUN = is.na) |>
      Reduce(f = `&`)
    if (is.logical(id)) {
      if (sum(id0) != length(id)) stop('bug!')
    } else if (is.integer(id)) {
      # have not thought of a way to check..
    }
    newV <- character(length = length(id0))
    if (is.logical(id)) {
      newV[id0] <- ifelse(test = id, yes = NA_character_, no = .labs[2L])
      # newV[!id0]; do nothing
    } else {
      newV[id0] <- NA_character_
      newV[id0][-id] <- .labs[2L]
    }
    cst <- data.frame(x@consort, newV = newV)
    attr(cst$newV, which = 'label') <- .crit
    nc <- length(x@consort) # old consort data
    names(cst)[nc+1L] <- sprintf(fmt = 'V%d', nc+1L) # attr-label kept :)
    ret@consort <- cst
    
  } else {
    
    cst <- data.frame(V1 = ifelse(test = id, yes = NA_character_, no = .labs[2L]))
    attr(cst$V1, which = 'label') <- .crit
    ret@consort <- cst
    
  }
  
  if (append.label) {
    ret@label <- c(ret@label, .labs[1L]) |>
      paste(collapse = '\n')
  }
  
  return(ret)
  
}


#' @title Sort \linkS4class{panel} by Given Criterion
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param y a one-sided \link[stats]{formula}
#' 
#' @param ... additional parameters of \link[base]{order}
#' 
#' @keywords internal
#' @export sort_by.panel
#' @export
sort_by.panel <- function(x, y, ...) {
  
  if (!inherits(y, what = 'formula') || length(y) != 2L) stop('`y` must be one-sided formula')
  
  if (!is.symbol(y[[2L]])) stop('Right-hand-side of `y` must be symbol')
  
  id <- y[[2L]] |> 
    as.character() |>
    call(name = _, quote(x)) |>
    eval() |>
    order(...) # e.g. `decreasing = TRUE`
  
  ret <- x[id, ] # `[.panel`
  
  if (length(x@consort)) {
    
    id0 <- x@consort |>
      lapply(FUN = is.na) |>
      Reduce(f = `&`)
    if (sum(id0) != length(id)) stop('bug!')
    cst <- x@consort
    cst[id0, ] <- cst[id0, , drop = FALSE][id, , drop = FALSE] # sort the previously eligible lines!!
    ret@consort <- cst
    
  } else {
    
    # do nothing!!
    # sorting does not change eligibility!!!
    
  }
  
  return(ret)
  
}



#' @title as_flextable.panel
#' 
#' @param x a \linkS4class{panel}
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @export as_flextable.panel
#' @export
as_flextable.panel <- function(x, ...) {
  
  data.frame(
    Collection = names(x@id),
    Variants = x@id |>
      vapply(FUN = paste, collapse = '\n', FUN.VALUE = '')
  ) |>
    flextable() |>
    autofit() |>
    hline()
    
}

