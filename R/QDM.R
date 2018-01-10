QDM <- function(o.c, m.c, m.p, ratio=FALSE, trace=0.05, trace.calc=0.5*trace,
         jitter.factor=0, n.tau=NULL, ratio.max=2, ratio.max.trace=10*trace,
         ECBC=FALSE, ties='first'){
  ################################################################################
  # MBC-QDM.R - Multivariate bias correction based on quantile delta mapping
  # and iterative application of Cholesky decomposition rescaling
  # Alex J. Cannon (alex.cannon@canada.ca)
  ################################################################################

  # Quantile delta mapping bias correction for preserving changes in quantiles
  # Note: QDM is equivalent to the equidistant and equiratio forms of quantile
  # mapping (Cannon et al., 2015).
  # Cannon, A.J., Sobie, S.R., & Murdock, T.Q. 2015. Bias Correction of
  # Simulated Precipitation by Quantile Mapping: How Well do Methods Preserve
  # Relative Changes in Quantiles and Extremes? Journal of Climate,
  # 28: 6938-6959. doi:10.1175/JCLI-D-14-00754.1
  #################################################################################
  # Modified to exclude missing values (NA) in o.c
  # By Hyung-Il Eum, Aug.2017
  #################################################################################



    # o = vector of observed values; m = vector of modelled values
    # c = current period;  p = projected period
    # ratio = TRUE --> preserve relative trends in a ratio variable (precipitation)
    # trace = 0.05 --> replace values less than trace with exact zeros
    # trace.calc = 0.5*trace --> treat values below trace.calc as censored
    # jitter.factor = 0.01 --> jitter to accommodate ties
    # n.tau = NULL --> number of empirical quantiles (NULL=sample length)
    # ratio.max = 2 --> maximum delta when values are less than ratio.max.trace
    # ratio.max.trace = 10*trace --> values below which ratio.max is applied
    # ECBC = TRUE --> apply Schaake shuffle to enforce o.c temporal sequencing
    #
    # tau.m-p = F.m-p(x.m-p)
    # delta.m = x.m-p {/,-} F.m-c^-1(tau.m-p)
    # xhat.m-p = F.o-c^-1(tau.m-p) {*,+} delta.m
    #
    # If jitter.factor > 0, apply a small amount of jitter to accommodate ties
    # due to limited measurement precision
    Not.Missing<-which(!is.na(o.c))
    o.c<-o.c[Not.Missing]
    if(jitter.factor > 0){
        o.c <- jitter(o.c, jitter.factor)
        m.c <- jitter(m.c, jitter.factor)
        m.p <- jitter(m.p, jitter.factor)
    }
    # For ratio data, treat exact zeros as left censored values less than
    # trace.calc
    if(ratio){
        epsilon <- .Machine$double.eps
        o.c[o.c < trace.calc] <- runif(sum(o.c < trace.calc), min=epsilon,
                                       max=trace.calc)
        m.c[m.c < trace.calc] <- runif(sum(m.c < trace.calc), min=epsilon,
                                       max=trace.calc)
        m.p[m.p < trace.calc] <- runif(sum(m.p < trace.calc), min=epsilon,
                                       max=trace.calc)
    }
    # Calculate empirical quantiles
    n <- length(m.p)
    if(is.null(n.tau)) n.tau <- n
    tau <- seq(0, 1, length=n.tau)
    quant.o.c <- quantile(o.c, tau)
    quant.m.c <- quantile(m.c, tau)
    quant.m.p <- quantile(m.p, tau)
    # Apply quantile delta mapping bias correction
    tau.m.p <- approx(quant.m.p, tau, m.p, rule=2)$y
    if(ratio){
        approx.t.qmc.tmp <- approx(tau, quant.m.c, tau.m.p, rule=2)$y
        delta.m <- m.p/approx.t.qmc.tmp
        delta.m[(delta.m > ratio.max) &
                (approx.t.qmc.tmp < ratio.max.trace)] <- ratio.max
        mhat.p <- approx(tau, quant.o.c, tau.m.p, rule=2)$y*delta.m
    } else{
        delta.m <- m.p - approx(tau, quant.m.c, tau.m.p, rule=2)$y
        mhat.p <- approx(tau, quant.o.c, tau.m.p, rule=2)$y + delta.m
    }
    mhat.c <- approx(quant.m.c, quant.o.c, m.c, rule=2)$y
    # For ratio data, set values less than trace to zero
    if(ratio){
        mhat.c[mhat.c < trace] <- 0
        mhat.p[mhat.p < trace] <- 0
    }
    if(ECBC){
        # empirical copula coupling/Schaake shuffle
        if(length(mhat.p)==length(o.c)){
            mhat.p <- sort(mhat.p)[rank(o.c, ties.method=ties)]
        } else{
            stop('Schaake shuffle failed due to incompatible lengths')
        }
    }
    list(mhat.c=mhat.c, mhat.p=mhat.p)
}