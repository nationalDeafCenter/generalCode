#### copied and modified from svyquantile() function in R survey package
#### options: method='linear', ci='probability'
#### works better with large data (e.g. don't need "design" object)

med1 <- function(x,w,wr,se){
     oo <- order(x)
     cum.w <- cumsum(w[oo])/sum(w)
    Qf<-approxfun(cum.w,x[oo],method='linear',f=1,
                  yleft=min(x),yright=max(x),
                  ties=min)

    point.est <-Qf(0.5)
    if(!se) return(point.est)
    estfun<-as.numeric(x<point.est)
    est <- sum(w*estfun)/sum(w)

    reps <- apply(wr,2,function(ww) sum(estfun*ww)/sum(ww))
    se <- sqrt(4*mean((reps-est)^2))
    ci <- Qf(c(est+2*se,est-2*se))
    SE <- ((ci[1]-ci[2])/4)
    c(median=point.est,SE=SE)
}

## finds median of x from ACS sdataset sdat
## weights are sdat$pwgtp
## replication weights are sdat$pwgtp[1-80]
med <- function(x,sdat,w1='pwgtp',wrep=paste0('pwgtp',1:80),se=TRUE){
    x <-model.frame(x,sdat)[[1]]
    w <- sdat[[w1]]
    if(!se) return(med1(x,w,se=FALSE))
    wr <- sdat[,wrep]
    out <- med1(x,w,wr,se=TRUE)
    c(out,n=nrow(sdat))
}


medStr <- function(x,w1='pwgtp',wrep=paste0('pwgtp',1:80),subst,sdat,...){
    x <- as.formula(paste('~',x))

    if(!missing(subst)){
        subst <- parse(text=subst)
        subst <- eval(subst,sdat)
        sdat <- sdat[subst, ]
    }
    med(x,sdat,w1=w1,wrep=wrep)
}
