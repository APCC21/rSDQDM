MBCr <-function(o.c, m.c, m.p, iter=20, cor.thresh=1e-4,
         ratio.seq=rep(FALSE, ncol(o.c)), trace=0.05,
         trace.calc=0.5*trace, jitter.factor=0, n.tau=NULL, ratio.max=2,
         ratio.max.trace=10*trace, ties='first', qmap.precalc=FALSE,
         silent=FALSE){
    # Multivariate quantile mapping bias correction (Spearman correlation)
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
    # Ordinal ranks of observed and modelled data
    o.c.r <- apply(o.c, 2, rank, ties.method=ties)
    m.c.r <- apply(m.c, 2, rank, ties.method=ties)
    m.p.r <- apply(m.p, 2, rank, ties.method=ties)
    m.c.i <- m.c.r
    if(cor.thresh > 0){
        # Spearman correlation to assess convergence
        cor.i <- cor(m.c.r)
        cor.i[is.na(cor.i)] <- 0
    } else{
        cor.diff <- 0
    }
    # Iterative MBC/reranking
    o.c.chol <- o.p.chol <- as.matrix(chol(nearPD(cov(o.c.r))$mat))
    for(i in seq(iter)){
        m.c.chol <- m.p.chol <- as.matrix(chol(nearPD(cov(m.c.r))$mat))
        fit.mbc <- MRS(o.c=o.c.r, m.c=m.c.r, m.p=m.p.r, o.c.chol=o.c.chol,
                       o.p.chol=o.p.chol, m.c.chol=m.c.chol, m.p.chol=m.p.chol)
        m.c.r <- apply(fit.mbc$mhat.c, 2, rank, ties.method=ties)
        m.p.r <- apply(fit.mbc$mhat.p, 2, rank, ties.method=ties)
        if(cor.thresh > 0){
            # Check on Spearman correlation convergence
            cor.j <- cor(m.c.r)
            cor.j[is.na(cor.j)] <- 0
            cor.diff <- mean(abs(cor.j-cor.i))
            cor.i <- cor.j
        }
        if(!silent){
            cat(i, mean(m.c.r==m.c.i), cor.diff, '')
        }
        if(cor.diff < cor.thresh) break
        if(identical(m.c.r, m.c.i)) break
        m.c.i <- m.c.r
    }
    if(!silent) cat('\n')
    for(i in seq(ncol(o.c))){
        # Replace ordinal ranks with QDM outputs
        m.c.r[,i] <- sort(m.c.qmap[,i])[m.c.r[,i]]
        m.p.r[,i] <- sort(m.p.qmap[,i])[m.p.r[,i]]
    }
    list(mhat.c=m.c.r, mhat.p=m.p.r)
}