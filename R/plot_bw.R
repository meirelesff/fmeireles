#' BW plot
#'
#' Function to plot the sensibility of RDD estimates to the bandwidth.
#'
#' @param x scores;
#' @param y Dependent variable;
#' @param c Cutpoint;
#' @param p Poly order (default 1);
#' @param triangular Triangular kernel? (default TRUE);
#' @param h_bw Vector with initial, final and breaks to calculte the bandwidths;
#' @param np Plot non-parametric estimates? (default FALSE);
#' @param cluster Cluster se.
#' @param err Type of error (default 'HC1').
#'
#' @import ggplot2
#' @import Formula
#'
#' @export
#'
#' @return A ggplot object;

plot_bw <- function(x, y, c = 0, p = 1, triangular = T, cluster = NULL, h_bw = NULL, np = FALSE, err = "HC1"){

  # Input tests
  if(!is.numeric(x) | !is.numeric(c)) stop("x and c must be numeric.") else xc <- x - c
  if(!is.numeric(h_bw) & !is.null(h_bw)) stop("h_bw must be numeric or NULL.")
  if(is.null(h_bw)){
    qnts <- as.numeric(quantile(abs(xc), probs = seq(0, 1, by = 0.1), na.rm = T))
    h_bw <- c(qnts[2], qnts[8], 50)
    if(sum(abs(x[!is.na(xc)]) < qnts[2]) < 50) stop("At least 50 intervals required. Or use custem h_bw values instead.")
  }
  if(length(h_bw) != 3) stop("h_bw must have length == 3.")
  if(length(x[!is.na(x)]) < h_bw[1]) stop("At least 50 intervals required.")
  h <- seq(h_bw[1], h_bw[2], length.out = h_bw[3])

  # Calculate the estimates
  coef <- ci_up <- ci_low <- numeric(length(h))
  for (i in 1:length(h)){
    reg <- rdd_loc(x = x, y = y, c = c, p = p, triangular = triangular, cluster = cluster, h = h[i], err = err)
    coef[i] <- reg$coef
    ci_up[i] <- reg$coef + 1.96 * reg$se
    ci_low[i] <- reg$coef - 1.96 * reg$se
  }

  # Plots
  out <- data.frame(coef = coef, ci_up = ci_up, ci_low = ci_low, h = h)
  brs <- round(as.numeric(quantile(h, probs = seq(0, 1, by = .2))), 1)

  p1 <- ggplot2::ggplot(out, ggplot2::aes(x = h, y = coef)) + ggplot2::geom_line(size = 0.82) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_up, ymax = ci_low), alpha = 0.2) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = brs) +
    ggplot2::geom_hline(yintercept = c, linetype = "dashed", size = 0.3)


  if(np){

    coef2 <- ci_up2 <- ci_low2 <- numeric(length(h))
    for (i in 1:length(h)) {
      reg <- rdd_loc(x = x, y = y, c = c, p = 0, triangular = triangular, cluster = cluster, h = h[i], err = err)
      coef2[i] <- reg$coef
      ci_up2[i] <- reg$coef + 1.96 * reg$se
      ci_low2[i] <- reg$coef - 1.96 * reg$se
    }

    out2 <- data.frame(coef = coef2, ci_up = ci_up2, ci_low = ci_low2, h = h)
    p1 <- p1 + ggplot2::geom_line(data = out2, aes(x = h, y = coef), size = 0.82, linetype = 2)
  }

  # Return
  p1
}
