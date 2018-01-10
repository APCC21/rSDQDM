MBCp <- function(o.c, m.c, m.p, iter=20, rerank=TRUE, cor.thresh=1e-4,
         ratio.seq=rep(FALSE, ncol(o.c)), trace=0.05, trace.calc=0.5*trace,
         jitter.factor=0, n.tau=NULL, ratio.max=2, ratio.max.trace=10*trace,
         ties='first', qmap.precalc=FALSE, silent=FALSE){
	# Multivariate quantile mapping bias correction (Pearson correlation)
    if(length(trace.calc)==1)
        trace.calc <- rep(trace.calc, ncol(o.c))
    if(length(trace)==1)
        trace <- rep(trace, ncol(o.c))
    if(length(jitter.factor)==1)
        jitter.factor <- rep(jitter.factor, ncol(o.c))
    if(length(ratio.max))
        ratio.max <- rep(ratio.max, ncol(o.c))
    if(length(ratio.max.trace)==1)
        ratio.max.trace <- rep(ratio.max.trace, ncol(o.c))
    m.c.qmap <- m.c
    m.p.qmap <- m.p
    if(!qmap.precalc){
        # Quantile delta mapping bias correction
        for(i in seq(ncol(o.c))){
            fit.qmap <- QDM(o.c=o.c[,i], m.c=m.c[,i], m.p=m.p[,i],
                            ratio=ratio.seq[i], trace.calc=trace.calc[i],
                            trace=trace[i], jitter.factor=jitter.factor[i],
                            n.tau=n.tau, ratio.max=ratio.max[i],
                            ratio.max.trace=ratio.max.trace[i])
            m.c.qmap[,i] <- fit.qmap$mhat.c
            m.p.qmap[,i] <- fit.qmap$mhat.p
        }
    }
    m.c <- m.c.qmap
    m.p <- m.p.qmap
    # Pearson correlation to assess convergence
    if(cor.thresh > 0){
        cor.i <- cor(m.c)
        cor.i[is.na(cor.i)] <- 0
    }
    o.c.chol <- o.p.chol <- as.matrix(chol(nearPD(cov(o.c))$mat))
    # Iterative MBC/QDM
    for(i in seq(iter)){
        m.c.chol <- m.p.chol <- as.matrix(chol(nearPD(cov(m.c))$mat))
        fit.mbc <- MRS(o.c=o.c, m.c=m.c, m.p=m.p, o.c.chol=o.c.chol,
                       o.p.chol=o.p.chol, m.c.chol=m.c.chol, m.p.chol=m.p.chol)
        m.c <- fit.mbc$mhat.c
        m.p <- fit.mbc$mhat.p
        for(j in seq(ncol(o.c))){
            fit.qmap <- QDM(o.c=o.c[,j], m.c=m.c[,j], m.p=m.p[,j], ratio=FALSE,
                            n.tau=n.tau)
            m.c[,j] <- fit.qmap$mhat.c
            m.p[,j] <- fit.qmap$mhat.p
        }
        # Check on Pearson correlation convergence
        if(cor.thresh > 0){
            cor.j <- cor(m.c)
            cor.j[is.na(cor.j)] <- 0
            cor.diff <- mean(abs(cor.j-cor.i))
            cor.i <- cor.j
        } else{
            cor.diff <- 0
        }
        if(!silent) cat(i, cor.diff, '')
        if(cor.diff < cor.thresh) break
    }
    if(!silent) cat('\n')
    if(rerank){
        # Replace with shuffled QDM elements
        for(i in seq(ncol(o.c))){
            m.c[,i] <- sort(m.c.qmap[,i])[rank(m.c[,i], ties.method=ties)]
            m.p[,i] <- sort(m.p.qmap[,i])[rank(m.p[,i], ties.method=ties)]
        }
    }
    list(mhat.c=m.c, mhat.p=m.p)
}
