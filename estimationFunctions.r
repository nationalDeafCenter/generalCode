## weighted mean of vector x, with weight vector w
svmean <- function(x,w,na.rm=TRUE){
    w <- w/sum(w)
    sum(x*w,na.rm=na.rm)
}

## estimates mean of x with SE
## wieghts w1 (vector), replicate weights wrep (matrix, Nx80)
## outputs estimated mean, se, and sample size
estSE <- function(x,w1,wrep,na.rm=TRUE){
    est <- svmean(x,w1,na.rm)

    reps <- apply(wrep,2,function(w) svmean(x,w,na.rm))

    se <- sqrt(mean((reps-est)^2)*4)

    n <- if(na.rm) sum(!is.na(x)) else length(x)

    c(est=est,se=se,n=n)
}

## estimates the mean of a variable defined by an expression expr
## optionally, on a subset of the population defined by a second expression subst
estExpr <- function(expr,subst,sdat,na.rm=TRUE){
    expr <- enquo(expr)

    if(!missing(subst)){
        subst <- enquo(subst)
        sdat <- filter(sdat,!!subst)
    }

    x <- transmute(sdat,x=!!expr)$x

    out <- estSE(x,sdat$pwgtp,sdat[,paste0('pwgtp',1:80)],na.rm)
    if(is.logical(x)) out[1:2] <- out[1:2]*100
    out
}

estSEstr <- function(x,w1='pwgtp',wrep=paste0('pwgtp',1:80),subst,sdat,na.rm=TRUE){

    if(!missing(subst)){
        subst <- parse(text=subst)
        subst <- eval(subst,sdat)
    } else subst <- NULL

    mn <- function(x,w1,subst,na.rm){
        if(!is.null(subst)){
            x <- x[subst]
            w1 <- w1[subst]
        }
        svmean(x,w1,na.rm=na.rm)
    }

    x <- eval(parse(text=x),sdat)

    est <- mn(x,sdat[[w1]],subst,na.rm=na.rm)

    reps <- vapply(wrep, function(ww) mn(x,sdat[[ww]],subst,na.rm=na.rm),1.0)

    se <- sqrt(mean((reps-est)^2)*4)

    n <- if(is.null(subst)){
             if(na.rm) sum(!is.na(x)) else nrow(sdat)
         } else if(na.rm) sum(!is.na(x[subst])) else sum(subst)
    c(est=est,se=se,n=n)
}

svby <- function(x,fac,subsets,FUN,sdat,w1='pwgtp',wrep=paste0('pwgtp',1:80),prop=TRUE,...){

    if(!missing(fac)){
        levs <- if(is.factor(sdat[[fac]])) levels(sdat[[fac]]) else sort(unique(sdat[[fac]]))
        subsets <- paste(fac,levs,sep='==')
    }

    out <- NULL
    for(subst in subsets){
        oo <- FUN(x=x,w1=w1,wrep=wrep,subst=subst,sdat=sdat,...)
        if(prop) oo <- c( p =svmean(eval(parse(text=subst),sdat),sdat[[w1]])*100,oo) ## proportion point estimate
        out <- rbind(out,oo)
    }
    if(prop) out[,'p'] <- out[,'p']/sum(out[,'p'])*100
    rownames(out) <- if(exists("levs"))  levs else subsets
    out
}

### count number of population in subset
svTot <- function(sdat,subst,w1='pwgtp',wrep=paste0('pwgtp',1:80)){
    if(!missing(subst)){
        subst <- parse(text=subst)
        subst <- eval(subst,sdat)
    } else subst <- rep(TRUE,nrow(sdat))

    stopifnot(is.logical(subst))

    if(any(is.na(subst))){
        warning('Deleting NAs in subset definition' )
        subst[is.na(subst)] <- FALSE
    }

    est <- sum(sdat[[w1]][subst])
    reps <- colSums(sdat[subst,wrep])
    se <- sqrt(4*mean((est-reps)^2))
    c(est,se)
}
