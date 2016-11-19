#' Non-parametric RDD
#'
#' Non-parametric RDD estimation
#'
#'
#' @param formula Formula
#' @param data Data
#' @param h Bandwidth
#' @param w weigths
#' @param var.name Variable name
#'
#' @import Formula
#' @import sandwich
#' @import stats
#'
#' @export

rdd_np <- function(formula, data, h = "All", w = NULL, var.name = "var"){


  formula <- Formula::as.Formula(formula)

  if(is.null(w)) mod <- lm(formula, data = data)
  else mod <- lm(formula, data = data, weights = w)

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

  if(pval < 0.05) coef_ast <- paste0(round(coef, 2), "*")
  else coef_ast <- round(coef, 2)

  res <- data.frame(var.name = var.name, coef = coef_ast, se = round(se, 2), pval = round(pval, 2), bw = h, N = N)
  res <- apply(res, 2, as.character)

  out <- list(res = res,
              coef = coef,
              se = se,
              pval = pval,
              h = h,
              mod = mod)

  out
}
