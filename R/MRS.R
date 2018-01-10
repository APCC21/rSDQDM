MRS <-function(o.c, m.c, m.p, o.c.chol=NULL, o.p.chol=NULL, m.c.chol=NULL,
         m.p.chol=NULL){

  # Multivariate rescaling based on Cholesky decomposition of the covariance
  #  matrix
  # Scheuer, E.M., Stoller, D.S., 1962. On the generation of normal random
  #  vectors. Technometrics, 4(2), 278-281.
  # Buerger, G., Schulla, J., & Werner, A.T. 2011. Estimates of future flow,
  #  including extremes, of the Columbia River headwaters. Wat Resour Res 47(10),
  #  W10520, doi:10.1029/2010WR009716

  ################################################################################
  # Multivariate bias correction based on iterative application of quantile
  # mapping (or ranking) and multivariate rescaling via Cholesky
  # decomposition of the covariance matrix. Results in simulated marginal
  # distributions and Pearson or Spearman rank correlations that match
  # observations
  # Scheuer, E.M., Stoller, D.S. 1962. On the generation of normal random
  #  vectors. Technometrics, 4(2), 278-281.
  # Iman, R.L., Conover, W.J. 1982. A distribution-free approach to inducing
  #  rank correlation among input variables. Communications in Statistics -
  #  Simulation and Computation 11(3), 311-334. doi:10.1080/03610918208812265
  # Buerger, G., Schulla, J., & Werner, A. T. 2011. Estimates of future flow,
  #  including extremes, of the Columbia River headwaters. Water Resources
  #  Research, 47(10), W10520, doi:10.1029/2010WR009716


    # Center based on multivariate means
    o.c.mean <- colMeans(o.c)
    m.c.mean <- colMeans(m.c)
    m.p.mean <- colMeans(m.p)
    o.c <- sweep(o.c, 2, o.c.mean, '-')
    m.c <- sweep(m.c, 2, m.c.mean, '-')
    m.p <- sweep(m.p, 2, m.p.mean, '-')
    # Cholesky decomposition of covariance matrix
    # If !is.null(o.p.chol) --> projected target
    if(is.null(o.c.chol)) o.c.chol <- chol(cov(o.c))
    if(is.null(o.p.chol)) o.p.chol <- chol(cov(o.c))
    if(is.null(m.c.chol)) m.c.chol <- chol(cov(m.c))
    if(is.null(m.p.chol)) m.p.chol <- chol(cov(m.c))
    # Bias correction factors
    mbcfactor <- solve(m.c.chol) %*% o.c.chol
    mbpfactor <- solve(m.p.chol) %*% o.p.chol
    # Multivariate bias correction
    mbc.c <- m.c %*% mbcfactor
    mbc.p <- m.p %*% mbpfactor
    # Recenter and account for change in means
    mbc.c <- sweep(mbc.c, 2, o.c.mean, '+')
    mbc.p <- sweep(mbc.p, 2, o.c.mean, '+')
    mbc.p <- sweep(mbc.p, 2, m.p.mean-m.c.mean, '+')
    list(mhat.c=mbc.c, mhat.p=mbc.p)
}