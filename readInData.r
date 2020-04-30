#### functions to read in ACS data from one year or 5-year dataset
#### assumes following directory structure:
## ndc/
## |--deafCenter/
## |  |--[working directory]/
## |--data/
## |  |--byYear/        single year ACS data
## |  |  |--ss08husa.csv    2 files for 2008 household level US data
## |  |  |--ss08husb.csv
## |  |  |--ss08pusa.csv    2 files for 2008 person level US data
## |  |  |--ss08pusa.csv
## |  |  |--ss08hpr.csv     2008 person and HH data for PR
## |  |  |--ss08ppr.csv
## |  |  ...                 data from 2009-2018 (or whatever years you need)
## |  |  |--ss18husa.csv     same file name structure, just e.g. 18 instead of 08
## |  |  ...
## |  |--acs5yr2018/   5-year ACS data from 2018 (i.e. 2014-2018)
## |  |  |--ss18husa.csv      4 files for 2018 household level US data
## |  |  |--ss18husb.csv
## |  |  |--ss18husc.csv
## |  |  |--ss18husd.csv
## |  |  |--ss18pusa.csv      4 files for 2018 person level US data
## |  |  |--ss18pusb.csv
## |  |  |--ss18pusc.csv
## |  |  |--ss18pusd.csv
## |  |  |--ss18hpr.csv      5-year PR data from 2018
## |  |  |--ss18ppr.csv
## |  |--acs5yr2017/   5-year ACS data from 2017 (i.e. 2013-2017)
## etc

require(tidyverse)

### utility function: returns "cols specification" telling
### read_csv which columns of data to read, and what type those are
### arguments:
### vars character vector of column names to read
### char (optional) which columns to read as characters (rest are integers)
chooseVars <- function(vars,char=NULL){
  if(!is.element('SERIALNO',vars)) vars <- c(vars,'SERIALNO')
  if(is.null(char)) char <- c('SERIALNO','NAICSP','FOD1P','OCCP','INDP')
  ctypes <- rep('i',length(vars))
  names(ctypes) <- vars
  charInt <- intersect(char,vars)
  if(length(charInt)) ctypes[charInt] <- 'c'
  ctypes$.default <- '_'
  do.call('cols',as.list(ctypes))
}

### read 1 ACS file
### arguments:
### path: path to csv file to read
### colTypes (optional): "cols specification" from chooseVars()
read1file <- function(path,colTypes,...){
  conv <- missing(colTypes)
  if(conv)
    colTypes <- cols(.default=col_character)
  dat <- read_csv(path,col_types=colTypes,...)
  if(conv) dat <- type_convert(dat)
  dat
}

read1type <- function(year,
                      oneORfive=c('one','five'),
                      HHorP=c('p','h'),
                      PRorUS=c('us','pr'),
                      colTypes,dir,...){
  if(is.null(dir)){
    dir <- if(oneORfive=='one') '../../data/byYear/' else paste0('../../data/acs5yr20',year)
  }

  FUN <- if(missing(colTypes)){
    function(part)
      read1file(paste0(dir,'ss',year,HHorP,PRorUS,part,'.csv'),...)
    } else function(part)
      read1file(paste0(dir,'ss',year,HHorP,PRorUS,part,'.csv'),colTypes=colTypes,...)

  lets <- if(PRorUS=='pr') '' else if(oneORfive=='one') c('a','b') else c('a','b','c','d')

  map_dfr(lets,FUN)
}

### reads 1-year or 5-year ACS data
### year is the year of the data (numeric or character, character prob better e.g. '18')
### oneORfive is 'one' for 1-year data or 'five' for 5-year
### pVars, hVars (optional) character vectors of column names to read in person and hh data. if Null, that dataset (i.e. person or hh) is not read.
### pr is TRUE if including puerto rico, FALSE otherwise
### dir overrides dir structure above
### char is a character vector of columns to read in as character type (otherwise they are all integer) default is c('SERIALNO','NAICSP','FOD1P','OCCP','INDP')
readACS <- function(year,oneORfive=c('one','five'),
                    pVars,hVars=NULL,pr=TRUE,dir,char=NULL,...){
  if(is.numeric(year)){
    if(year<10){
      year <- paste0('0',year)
    } else if(year>2000){
      year <- substr(as.character(year),3,4)
    } else year <- as.charactr(year)
  }

  if(missing(dir))
    dir <- NULL

  dat <- list(us=list(),pr=list())

  for(HHorP in c('p','h')){
    for(PRorUS in if(pr) c('us','pr') else 'us'){
      vars <- get(paste0(HHorP,'Vars'),envir=environment())
      if(!missing(vars)){
        colTypes <- chooseVars(vars,char)
        dat[[PRorUS]][[HHorP]] <-
          if(is.null(vars)) NULL else read1type(year=year,
                      oneORfive=oneORfive,
                      HHorP=HHorP,
                      PRorUS=PRorUS,
                      colTypes=colTypes,dir=dir,...)
      } else
        dat[[PRorUS]][[HHorP]] <- read1type(year=year,
                      oneORfive=oneORfive,
                      HHorP=HHorP,
                      PRorUS=PRorUS,
                      dir=dir,...)
    }
  }

  ### merge (actually "left join" sdat and hdat
  ### however, left_join() apparently takes up a ton of RAM and overloads the computer, whereas this works quick and easy
### that said, I'm always nervous about match() cuz it has led to bugs before. Hence the check below
  dat <- if(!is.null(hVars)&!is.null(pVars)){
      map(dat,
          function(dd){
              hdatBind <- dd$h[match(dd$p$SERIALNO,dd$h$SERIALNO),]
              stopifnot(all.equal(hdatBind$SERIALNO,dd$p$SERIALNO))
              sdat <- bind_cols(dd$p,hdatBind)
              sdat
          }
          )
  } else if(is.null(hVars)&!is.null(pVars)) map(dat,~.$p) else if(is.null(pVars)&!is.null(hVars)) map(dat,~.$p)

  if(pr) bind_rows(dat$us,dat$pr) else dat$us
}
