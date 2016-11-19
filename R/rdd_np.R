#' Non-parametric RDD
#'
#' Non-parametric RDD estimation
#'
#'
#' @param formula Formula
#' @param data Data
#' @param h Bandwidth
#'
#' @import Formula
#' @import sandwich
#' @import stats
#'
#' @export

rdd_np <- function(formula, data, h = "All"){


  formula <- Formula::as.Formula(formula)
  mod <- lm(formula, data = data)

  coef <- as.numeric(coef(mod)[2])
  se <- as.numeric(sqrt(diag(sandwich::vcovHC(mod, type = "HC1")))[2])
  ci_low <- round(coef - 1.96 * se, 2)
  ci_up <- round(coef + 1.96 * se, 2)
  N <- sum(summary(mod)$df[1:2])

  if (!is.numeric(se))
    stop("SE wrong.")
  if (!is.numeric(coef))
    stop("COEF wrong.")

  pval <- 2 * stats::pnorm(abs(coef / se), lower.tail = F)
  res <- data.frame(coef = round(coef, 2), se = round(se, 2), pval = round(pval, 2), h = h, N = N)

  out <- list(res = res,
              coef = coef,
              se = se,
              pval = pval)

  out
}
