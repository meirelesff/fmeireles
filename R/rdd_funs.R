#' rdrobust wrapper
#'
#' A wrapper function to estimate multiple rdds.
#'
#'
#' @param y Dependent variable
#' @param x Forcing variable
#' @param rep Report all results or just the conventional ones?
#' @param ... Aditional arguments
#'
#' @importFrom rdrobust rdrobust
#'
#' @export

rdd <- function(y, x, rep = c("all", "conv"), ...){


  rep <- match.arg(rep)
  mod <- rdrobust::rdrobust(y = y, x = x, ...)
  out <- as.data.frame(rdrobust::summary.rdrobust(mod)$coefficients)[, c(1, 2, 4)]

  if(is.null(mod$h)) out$h <- "all"
  else out$h <- mod$h

  out$N <- mod$N_h_l + mod$N_h_r

  names(out) <- c("coef", "sd", "p-value", "h", "N")
  if(rep == "conv") return(out[1, ])
  else return(out)
}



#' Check RDD
#'
#' A wrapper function to estimate multiple rdds with different polynomials.
#'
#'
#' @param y Dependent variable
#' @param x Forcing variable
#' @param ... Aditional arguments
#'
#' @importFrom rdrobust rdrobust
#' @importFrom dplyr bind_rows mutate %>%
#'
#' @export

check_rdd <- function(y, x, ...){

  lapply(1:5, function(p) rdd(x = x, y = y, rep = "conv", p = p, q = p + 1, ...)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(poly = rownames(.))
}



#' Triangular kernel
#'
#' Calculate triagular kernel weigths.
#'
#'
#' @param x A numeric vector
#' @param h Bandwidth
#' @param c Cutoff
#'
#' @export

triangular <- function(x, h, c){

  if(!is.numeric(x)) stop("x must be numeric.")
  1 - abs(x - c) / h
}
