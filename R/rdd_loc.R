#' RDD local
#'
#' Estimate local RDD.
#'
#' @param x Scores;
#' @param y Dependent variable;
#' @param c Cutpoint;
#' @param cluster Cluster se;
#' @param p Poly order (default 1);
#' @param bw Bandwitdh selector (default mserd);
#' @param var.name Variable name;
#' @param h Bandwidth. If filled, \code{bw} is ignored;
#' @param triangular Triangular kernel? (default TRUE).
#'
#' @import lmtest
#' @import sandwich
#' @import rdrobust
#' @import stats
#'
#' @export
#'
#' @return A list object.

rdd_loc <- function(x, y, c = 0, cluster = NULL, p = 1, bw = "mserd", var.name = "var", h = NULL, triangular = T){

  # tests the inputs
  if(!is.numeric(x) | !is.numeric(y)) stop("x e y must be numeric.")
  if(!p %in% c(1:5)) stop("p must be between 1 and 5.")
  xrange <- range(x, na.rm = T)
  if(c < xrange[1] | c > xrange[2]) stop("c is out of x.")
  if(!is.character(var.name) & !is.null(var.name)) stop("var.name must be character.")
  if(!is.logical(triangular)) stop("triangular must be logical.")

  # cleans the data
  if(!is.null(cluster)) data <- data.frame(x = x, y = y, cluster = cluster)
  else data <- data.frame(x = x, y = y)
  data <- data[complete.cases(data),]
  data$x <- data$x - c

  # tests the h and cuts the data
  if(is.null(h)) h <- rdrobust::rdbwselect(y = data$y, x = data$x, c = 0, p = p, q = p + 1, bwselect = bw)$bws[1]
  else {
    if(h < xrange[1] | h > xrange[2]) stop("h is out of x.")
    h <- abs(h)
  }

  data <- data[data$x > -h & data$x < h,]
  if(!is.null(cluster)) data$cluster <- as.character(data$cluster)
  data$treat <- data$x >= 0
  if(length(unique(data$treat)) != 2) stop("There is not variation in the treatment.")

  med_controle <- round(mean(data$y[!data$treat]), 2)
  med_trat<- round(mean(data$y[data$treat]), 2)

  if(triangular) weights <- 1 - abs(data$x) / h
  else weights <- NULL

  if(p == 1) reg <- lm(y ~ treat * x, data = data, weights = weights)
  else if(p == 2) reg <- lm(y ~ treat*x + treat*I(x^2), data = data, weights = weights)
  else if(p == 3) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3), data = data, weights = weights)
  else if(p == 4) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3) + treat*I(x^4), data = data, weights = weights)
  else if(p == 5) reg <- lm(y ~ treat*x + treat*I(x^2) + treat*I(x^3) + treat*I(x^4) + treat*I(x^5), data = data, weights = weights)

  coef <- as.numeric(coef(reg)[2])
  if(!is.null(cluster)){

    cluster_se <- function(model, cluster) {
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- model$rank
      dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
      uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
      rcse.cov <- dfc * sandwich::sandwich(model, meat. = crossprod(uj)/N)
      rcse.se <- lmtest::coeftest(model, rcse.cov)
      return(rcse.se[2, 2])
    }

    se <- cluster_se(model = reg, cluster = cluster)
  }
  else se <- as.numeric(sqrt(diag(sandwich::vcovHC(reg, type = "HC1")))[2])
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
