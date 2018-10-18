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

    c(est,se,n)
}

## estimates the mean of a variable defined by an expression expr
## optionally, on a subset of the population defined by a second expression subst
estExpr <- function(expr,subst,na.rm=TRUE){
    expr <- enquo(expr)

    if(!missing(subst)){
        subst <- enquo(subst)
        sdat <- filter(sdat,!!subst)
    }

    x <- transmute(sdat,x=!!expr)$x

    estSE(x,sdat$PWGTP,sdat[,paste0('pwgtp',1:80)],na.rm)
}
