#' RD simulation data
#'
#' Simulate data for RDD
#'
#' @param tau Size of the descontinuity
#' @param N Sample size
#' @param c Cutoff
#' @param beta0 Intercept
#' @param beta1 Beta 1
#' @param beta2 Beta 2
#' @param ruido Noise
#'
#' @importFrom stats sd rnorm
#'
#' @export

rd_sim <- function(tau = 1, N = 1000, c = NULL, beta0 = 0, beta1 = 1, beta2 = 1, ruido = 1){


  if(is.null(c)) c <- N %/% 2
  scores <- 1:N
  scores <- scores - c
  y.l <- beta0 + beta1 * scores / stats::sd(scores)
  y.r <- tau + beta2 * y.l[scores >= 0]
  y <- c(y.l[scores < 0], y.r) + stats::rnorm(N, 0, ruido)
  data.frame(x = scores, y = y)
}
