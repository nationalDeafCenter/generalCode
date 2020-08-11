## weighted mean of vector x, with weight vector w
svmean <- function(x,w,na.rm=TRUE){
    if(na.rm){
        w <- w[!is.na(x)]
        x <- x[!is.na(x)]
    }
    w <- w/sum(w)
    sum(x*w)
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
estExpr <- function(expr,subst,sdat,w1name='pwgtp',wrepname=paste0(w1name,1:80),na.rm=TRUE){
    expr <- enquo(expr)

    if(!missing(subst)){
        subst <- enquo(subst)
        sdat <- filter(sdat,!!subst)
    }

    x <- transmute(sdat,x=!!expr)$x

    out <- estSE(x,sdat[[w1name]],sdat[,wrepname],na.rm)
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
    n=sum(subst)
    c(est=est,se=se,n=n)
}

## estimate proportion of each category of x (character--name of factor in data)
factorProps <- function(fac,sdat,w1='pwgtp',wrep=paste0('pwgtp',1:80),cum,...){
    levs <- if(is.factor(sdat[[fac]])) levels(sdat[[fac]]) else sort(unique(sdat[[fac]]))
    levs2 <- if(is.character(levs)) paste0("'",levs,"'") else levs

    if(missing(cum)) cum <- is.ordered(sdat[[fac]])|is.numeric(sdat[[fac]])

    subsets <- if(cum) c(paste0(fac,'==',levs2[1]),paste(fac,levs2[-1],sep='>='))
               else paste(fac,levs2,sep='==')

    if(length(levs)==2){ #maybe save a bit of time
        s1 <- estSEstr(subsets[1], w1=w1,wrep=wrep,sdat=sdat)
        s2 <- setNames(c(1-s1['est'],s1['se'],s1['n']),names(s1))
        out <- list(s1,s2)
    } else{
        out <- lapply(subsets,estSEstr,w1=w1,wrep=wrep,sdat=sdat)
    }

    if(cum) levs[-c(1,length(levs))] <- paste0('>=',levs[-c(1,length(levs))])
    for(i in 1:length(out)){
        out[[i]] <- out[[i]][c('est','se')]*100
        names(out[[i]]) <- c(paste('%',levs[i]),paste0(levs[i],' SE'))
    }

    out <- do.call('c',out)
    out <- c(out,n=nrow(sdat))
    out
}


### wraps around dplyr "do()" string of commands
### so that it's a readable data.frame output
### usage e.g. out <- FIX(dat%>%group_by(deaf)%>%do(x=something))
FIX <- function(tib){
    lst <- sapply(tib,is.list)

    out <- NULL
    for(nn in names(tib)[lst]){
        out <- cbind(out,
                     do.call('rbind',tib[[nn]]))
    }
    out <- cbind(tib[,!lst],out)
    out[sapply(out,is.factor)] <- sapply(out[sapply(out,is.factor)],as.character)
    #names(out)[1:sum(!lst)] <- ''

    out <- as.data.frame(lapply(out,as.vector))

    out
}
