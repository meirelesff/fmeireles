#' RDD plots
#'
#' A function to create RDD plots.
#'
#' @param x Forcing variable
#' @param y Dependent variable
#' @param c Cutoff
#' @param p Polynomial order
#' @param numbinl Bins to the left
#' @param numbinr Bins to the right
#' @param binselect Method to select bins
#' @param x.lim Limits on X axis
#' @param y.lim Limits on Y axis
#' @param lowerend Lower end
#' @param upperend Upper end
#' @param scale Scale
#' @param scalel Scale to the left
#' @param scaler Scale to the right
#' @param ... Further arguments
#'
#' @import ggplot2
#' @importFrom Formula as.Formula
#' @importFrom stats complete.cases lm quantile var
#'
#' @return A ggplot2 object
#'
#' @export

rd_plot <- function(x, y, c = 0, p = 2, numbinl = NULL, numbinr = NULL, binselect = "esmv",
                   x.lim = NULL, y.lim = NULL, lowerend = NULL, upperend = NULL, scale = 1, scalel = 1,scaler = 1, ...) {

  call <- match.call()
  if(!is.null(x.lim)) na.ok <- stats::complete.cases(x) & stats::complete.cases(y) & x > x.lim[1] & x < x.lim[2]
  else na.ok <- stats::complete.cases(x) & stats::complete.cases(y)

  x <- x[na.ok]
  y <- y[na.ok]

  if (is.null(lowerend)) {
    lowerend = min(x)
  }
  if (is.null(upperend)) {
    upperend = max(x)
  }
  x_low = lowerend
  x_upp = upperend

  size=sum(x>=x_low & x<=x_upp)
  y=y[x>=x_low & x<=x_upp]
  x=x[x>=x_low & x<=x_upp]

  x_l = x[x<c]; x_r = x[x>=c]
  y_l = y[x<c];	y_r = y[x>=c]
  x.min = min(x);	x.max = max(x)
  range_l = max(x_l) - min(x_l)
  n_l = length(x_l)
  range_r = max(x_r) - min(x_r)
  n_r = length(x_r)
  n = n_l + n_r
  meth="es"
  #####********************* ERRORS
  exit=0
  if (c<=x.min | c>=x.max){
    print("c should be set within the range of x")
    exit = 1
  }

  if (p<=0 ){
    print("p should be a positive number")
    exit = 1
  }

  if (scale<=0 |scalel<=0 |scaler<=0){
    print("scale should be a positive number")
    exit = 1
  }

  p_ceiling = ceiling(p)/p

  if (p_ceiling!=1) {
    print("p should be an integer number")
    exit = 1
  }

  if (exit>0) {
    stop()
  }

  p1 = p+1
  compute =0

  rp_l = matrix(NA,n_l,p+1);  rp_r = matrix(NA,n_r,p+1)
  for (j in 1:p1) {
    rp_l[,j] = x_l^(j-1)
    rp_r[,j] = x_r^(j-1)
  }
  gamma_p1_l = stats::lm(y_l~rp_l-1)$coeff
  gamma_p1_r = stats::lm(y_r~rp_r-1)$coeff

  mu0_p1_l = rp_l%*%gamma_p1_l;	mu0_p1_r = rp_r%*%gamma_p1_r

  J_star_orig=c(numbinl, numbinr)

  y_l.sq = y_l^2
  y_r.sq = y_r^2
  gamma_p2_l = stats::lm(y_l.sq~rp_l-1)$coeff
  gamma_p2_r = stats::lm(y_r.sq~rp_r-1)$coeff

  ### Bias w/sample

  drp_l = matrix(NA,n_l,p);	drp_r = matrix(NA,n_r,p)
  for (j in 1:p) {
    drp_l[,j] = j*x_l^(j-1)
    drp_r[,j] = j*x_r^(j-1)
  }
  mu1_hat_l = drp_l%*%(gamma_p1_l[2:p1])
  mu1_hat_r = drp_r%*%(gamma_p1_r[2:p1])

  ######################### ES
  ind.l = order(x_l); ind.r = order(x_r)
  x.i.l = x_l[ind.l]; x.i.r = x_r[ind.r]
  y.i.l = y_l[ind.l]; y.i.r = y_r[ind.r]

  dxi.l=(x.i.l[2:length(x.i.l)]-x.i.l[1:(length(x.i.l)-1)])
  dxi.r=(x.i.r[2:length(x.i.r)]-x.i.r[1:(length(x.i.r)-1)])
  dyi.l=(y.i.l[2:length(y.i.l)]-y.i.l[1:(length(y.i.l)-1)])
  dyi.r=(y.i.r[2:length(y.i.r)]-y.i.r[1:(length(y.i.r)-1)])

  x.bar.i.l = (x.i.l[2:length(x.i.l)]+x.i.l[1:(length(x.i.l)-1)])/2
  x.bar.i.r = (x.i.r[2:length(x.i.r)]+x.i.r[1:(length(x.i.r)-1)])/2
  rp.i_l  = matrix(NA,n_l-1,p+1); rp.i_r= matrix(NA,n_r-1,p+1)
  drp.i_l = matrix(NA,n_l-1,p); drp.i_r = matrix(NA,n_r-1,p)

  for (j in 1:p1) {
    rp.i_l[,j] = x.bar.i.l^(j-1)
    rp.i_r[,j] = x.bar.i.r^(j-1)
  }

  for (j in 1:p) {
    drp.i_l[,j] = j*x.bar.i.l^(j-1)
    drp.i_r[,j] = j*x.bar.i.r^(j-1)
  }

  mu0.i_hat_l = rp.i_l%*%gamma_p1_l;  mu0.i_hat_r = rp.i_r%*%gamma_p1_r
  mu2.i_hat_l = rp.i_l%*%gamma_p2_l;  mu2.i_hat_r = rp.i_r%*%gamma_p2_r

  mu0_hat_l = rp_l%*%gamma_p1_l;  mu0_hat_r = rp_r%*%gamma_p1_r
  mu2_hat_l = rp_l%*%gamma_p2_l;  mu2_hat_r = rp_r%*%gamma_p2_r

  mu1.i_hat_l = drp.i_l%*%(gamma_p1_l[2:p1]);  mu1.i_hat_r = drp.i_r%*%(gamma_p1_r[2:p1])

  sigma2_hat_l.bar = mu2.i_hat_l - mu0.i_hat_l^2
  sigma2_hat_r.bar = mu2.i_hat_r - mu0.i_hat_r^2

  sigma2_hat_l = mu2_hat_l - mu0_hat_l^2
  sigma2_hat_r = mu2_hat_r - mu0_hat_r^2

  J.fun = function(B,V) {ceiling((((2*B)/V)*n)^(1/3))}
  var.y_l = var(y_l)
  var.y_r = var(y_r)

  B.es.hat.dw = c( ((c-x.min)^2/(12*n))*sum(mu1_hat_l^2),((x.max-c)^2/(12*n))*sum(mu1_hat_r^2))
  V.es.hat.dw = c((0.5/(c-x.min))*sum(dxi.l*dyi.l^2),(0.5/(x.max-c))*sum(dxi.r*dyi.r^2))
  V.es.chk.dw = c((1/(c-x.min))*sum(dxi.l*sigma2_hat_l.bar),(1/(x.max-c))*sum(dxi.r*sigma2_hat_r.bar))
  J.es.hat.dw = J.fun(B.es.hat.dw, V.es.hat.dw)
  J.es.chk.dw = J.fun(B.es.hat.dw, V.es.chk.dw)

  B.qs.hat.dw = c((n_l^2/(24*n))*sum(dxi.l^2*mu1.i_hat_l^2), (n_r^2/(24*n))*sum(dxi.r^2*mu1.i_hat_r^2))
  V.qs.hat.dw = c((1/(2*n_l))*sum(dyi.l^2),(1/(2*n_r))*sum(dyi.r^2))
  V.qs.chk.dw = c((1/n_l)*sum(sigma2_hat_l), (1/n_r)*sum(sigma2_hat_r))
  J.qs.hat.dw = J.fun(B.qs.hat.dw, V.qs.hat.dw)
  J.qs.chk.dw = J.fun(B.qs.hat.dw, V.qs.chk.dw)

  J.es.hat.mv  = c(ceiling((var.y_l/V.es.hat.dw[1])*(n/log(n)^2)), ceiling((var.y_r/V.es.hat.dw[2])*(n/log(n)^2)))
  J.es.chk.mv  = c(ceiling((var.y_l/V.es.chk.dw[1])*(n/log(n)^2)), ceiling((var.y_r/V.es.chk.dw[2])*(n/log(n)^2)))
  J.qs.hat.mv  = c(ceiling((var.y_l/V.qs.hat.dw[1])*(n/log(n)^2)), ceiling((var.y_r/V.qs.hat.dw[2])*(n/log(n)^2)))
  J.qs.chk.mv  = c(ceiling((var.y_l/V.qs.chk.dw[1])*(n/log(n)^2)), ceiling((var.y_r/V.qs.chk.dw[2])*(n/log(n)^2)))

  if (binselect=="es") {
    J_star_orig = J.es.hat.dw
    meth="es"
    binselect_type="IMSE-optimal evenly-spaced method using spacings estimators"
    J_IMSE = J.es.hat.dw
    J_MV   = J.es.hat.mv
  }

  if (binselect=="espr") {
    J_star_orig = J.es.chk.dw
    meth="es"
    binselect_type="IMSE-optimal evenly-spaced method using polynomial regression"
    J_IMSE = J.es.chk.dw
    J_MV   = J.es.chk.mv
  }

  if (binselect=="esmv" ) {
    J_star_orig = J.es.hat.mv
    meth="es"
    binselect_type="mimicking variance evenly-spaced method using spacings estimators"
    J_IMSE = J.es.hat.dw
    J_MV   = J.es.hat.mv
  }

  if (binselect=="esmvpr" ) {
    J_star_orig = J.es.chk.mv
    meth="es"
    binselect_type="mimicking variance evenly-spaced method using polynomial regression"
    J_IMSE = J.es.chk.dw
    J_MV   = J.es.chk.mv
  }


  if (binselect=="qs" ) {
    J_star_orig = J.qs.hat.dw
    meth="qs"
    binselect_type="IMSE-optimal quantile-spaced method using spacings estimators"
    J_IMSE = J.qs.hat.dw
    J_MV   = J.qs.hat.mv
  }

  if (binselect=="qspr" ) {
    J_star_orig = J.qs.chk.dw
    meth="qs"
    binselect_type="IMSE-optimal quantile-spaced method using polynomial regression"
    J_IMSE = J.qs.chk.dw
    J_MV   = J.qs.chk.mv
  }
  if (binselect=="qsmv" ) {
    J_star_orig = J.qs.hat.mv
    meth="qs"
    binselect_type="mimicking variance quantile-spaced method using spacings estimators"
    J_IMSE = J.qs.hat.dw
    J_MV   = J.qs.hat.mv
  }


  if (binselect=="qsmvpr" ) {
    J_star_orig = J.qs.chk.mv
    meth="qs"
    binselect_type="mimicking variance quantile-spaced method using polynomial regression"
    J_IMSE = J.qs.chk.dw
    J_MV   = J.qs.chk.mv
  }

  if (scale>1 & scalel==1 & scaler==1){
    scalel=scaler=scale
  }

  J_star_l = scalel*J_star_orig[1]
  J_star_r = scaler*J_star_orig[2]

  if (!is.null(numbinl)&!is.null(numbinr)) {
    J_star_l = numbinl
    J_star_r = numbinr
    binselect_type="manually evenly spaced"
  }


  scale_l = J_star_l / J_IMSE[1]
  scale_r = J_star_r / J_IMSE[2]

  bin_x_l = rep(0,length(x_l)); bin_x_r = rep(0,length(x_r))
  jump_l = range_l/J_star_l;jump_r = range_r/J_star_r;

  if (meth=="es") {
    jumps_l=seq(min(x_l),max(x_l),jump_l)
    jumps_r=seq(min(x_r),max(x_r),jump_r)
    #binselect_type="Evenly-Spaced"
  }
  else if (meth=="qs") {
    jumps_l=quantile(x_l,probs=seq(0,1,1/J_star_l))
    jumps_r=quantile(x_r,probs=seq(0,1,1/J_star_r))
    # binselect_type="Quantile-Spaced"
  }

  for (k in 1:(J_star_l-1)) {
    bin_x_l[x_l>=jumps_l[k] & x_l<jumps_l[k+1]] = -J_star_l+k-1
  }
  bin_x_l[x_l>=jumps_l[(J_star_l)]] = -1

  for (k in 1:(J_star_r-1)) {
    bin_x_r[x_r>=jumps_r[k] & x_r<jumps_r[k+1]] = k
  }
  bin_x_r[x_r>=jumps_r[(J_star_r)]] = J_star_r

  bin_xlmean=bin_ylmean=rep(0,J_star_l)
  bin_xrmean=bin_yrmean=rep(0,J_star_r)
  for (k in 1:(J_star_l)) {
    bin_xlmean[k]=mean(c(jumps_l[k],jumps_l[k+1]))
    #bin_xlmean[k]=mean(x_l[bin_x_l==-k])
    bin_ylmean[J_star_l-k+1]=mean(y_l[bin_x_l==-k])
  }
  for (k in 1:(J_star_r)) {
    bin_xrmean[k]=mean(c(jumps_r[k],jumps_r[k+1]))
    #bin_xrmean[k]=mean(x_r[bin_x_r==k])
    bin_yrmean[k]=mean(y_r[bin_x_r==k])
  }

  bin_x=c(bin_x_l,bin_x_r)
  bin_xmean=c(bin_xlmean,bin_xrmean)
  bin_ymean=c(bin_ylmean,bin_yrmean)
  x_sup = c(x_l, x_r)
  #y_hat = c(mu0_p1_l, mu0_p1_r)

  if (is.null(x.lim)){
    x.lim=c(min(x_l),max(x_r))
  }

  if (is.null(y.lim)){
    y.lim=c(min(c(y_l,y_r)),max(c(y_l,y_r)))
  }

  treat <- 0
  dados <- data.frame(x = bin_xmean[order(bin_xmean)], y = bin_ymean[order(bin_xmean)], treat = treat)
  dados$treat[dados$x >= c] <- 1

  if(p == 1) forms <- as.Formula(y ~ x)
  else if(p == 2) forms <- as.Formula(y ~ x + I(x^2))
  else if(p == 3) forms <- as.Formula(y ~ x + I(x^2) + I(x^3))
  else if(p == 4) forms <- as.Formula(y ~ x + I(x^2) + I(x^3) + I(x^4))

  ggplot2::ggplot(dados, aes(x, y, group = factor(treat))) + ggplot2::stat_smooth(method = "lm", formula = forms, se = F, colour = "black", alpha = 0.2, size = .82) +
    ggplot2::geom_point(size = 1.1, color = "gray5") + ggplot2::geom_vline(xintercept = c, linetype = 2, size = 0.4) + ggplot2::scale_y_continuous(expand = c(0, 0), limits = y.lim) +
    ggplot2::scale_x_continuous(expand = c(0, 0))
}
