#' RDD local with fixed effects
#'
#' Estimate local RDD (or non-parametric) with two FE.
#'
#' @param x Scores;
#' @param y Dependent variable;
#' @param c Cutpoint;
#' @param fe fixed effect indicator;
#' @param cluster Cluster se;
#' @param p Poly order (default 1; 0 for non-parametric);
#' @param bw Bandwitdh selector (default mserd);
#' @param var.name Variable name;
#' @param h Bandwidth. If filled, \code{bw} is ignored;
#' @param triangular Triangular kernel? (default TRUE).
#' @param err Type of error (default 'HC1').
#'
#' @import multiwayvcov
#' @import lmtest
#' @import sandwich
#' @import rdrobust
#' @import stats
#'
#' @export
#'
#' @return A list object.

rdd_fe <- function(x, y, c = 0, cluster = NULL, p = 1, bw = "mserd", var.name = "var", h = NULL, triangular = T, err = "HC1", fe = NULL){

  # tests the inputs
  if(is.null(fe)) stop("FE must be provided.")
  if(!is.numeric(x) | !is.numeric(y)) stop("x e y must be numeric.")
  if(!p %in% c(0:5)) stop("p must be between 1 and 5.")
  xrange <- range(x, na.rm = T)
  if(c < xrange[1] | c > xrange[2]) stop("c is out of x.")
  if(!is.character(var.name) & !is.null(var.name)) stop("var.name must be character.")
  if(!is.logical(triangular)) stop("triangular must be logical.")
  if(!err %in% c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")) stop("Invalid error type.")

  # cleans the data
  if(!is.null(cluster)) data <- data.frame(x = x, y = y, cluster = as.character(cluster), fe = as.character(fe))
  else data <- data.frame(x = x, y = y, fe1 = as.character(fe))
  data <- data[complete.cases(data),]
  data$x <- data$x - c

  # tests the h and cuts the data
  if(is.null(h)) h <- rdrobust::rdbwselect(y = data$y, x = data$x, c = 0, p = p, q = p + 1, bwselect = bw)$bws[1]
  else {
    if(h < xrange[1] | h > xrange[2]) stop("h is out of x.")
    h <- abs(h)
  }

  data <- data[data$x > -h & data$x < h,]
  data$treat <- data$x >= 0
  if(length(unique(data$treat)) != 2) stop("There is not variation in the treatment.")

  med_controle <- round(mean(data$y[!data$treat]), 2)
  med_trat<- round(mean(data$y[data$treat]), 2)

  if(triangular) weights <- 1 - abs(data$x) / h
  else weights <- NULL

  if(p == 0) reg <- lm(y ~ treat + fe - 1, data = data, weights = weights)
  else if(p == 1) reg <- lm(y ~ treat * x + fe - 1, data = data, weights = weights)
  else if(p == 2) reg <- lm(y ~ treat*x + treat*I(x^2) + fe - 1, data = data, weights = weights)
  else if(p == 3) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3) + fe - 1, data = data, weights = weights)
  else if(p == 4) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3) + treat*I(x^4) + fe - 1, data = data, weights = weights)
  else if(p == 5) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3) + treat*I(x^4) + treat*I(x^5) + fe - 1, data = data, weights = weights)

  coef <- as.numeric(coef(reg)[2])
  if(!is.null(cluster)){

    se <- multiwayvcov::cluster.vcov(reg, data$cluster)
    se <- lmtest::coeftest(reg, se)[2, 2]
  }
  else se <- as.numeric(sqrt(diag(sandwich::vcovHC(reg, type = err)))[2])
  ci_low <- round(coef - 1.96 * se, 2)
  ci_up <- round(coef + 1.96 * se, 2)
  N <- sum(summary(reg)$df[1:2])
  if(!is.numeric(se)) stop("SE wrong.")
  if(!is.numeric(coef)) stop("COEF wrong.")
  pval <- 2 * stats::pnorm(abs(coef/se), lower.tail = F)

  if(pval < 0.05) coef_ast <- paste0(round(coef, 2), "*")
  else coef_ast <- round(coef, 2)

  res <- data.frame(var.name = var.name, med_trat = med_trat, med_cont = med_controle, dif = med_trat - med_controle,  coef = coef_ast, se = round(se, 2), ci = paste0("[", ci_low, ", ", ci_up, "]"), pval = round(pval, 2), bw = round(h, 2), N = N, stringsAsFactors = F)
  res <- apply(res, 2, as.character)
  out <- list(res = res, coef = coef, se = se, pval = pval, N = N, bw = h)
  out
}
