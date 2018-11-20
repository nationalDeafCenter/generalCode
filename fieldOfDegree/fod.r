### step 1: downloaded 2017_ACS_Code_Lists.pdf from
### https://www2.census.gov/programs-surveys/acs/tech_docs/code_lists/2017_ACS_Code_Lists.pdf

### step 2: for each (minor) category, copied text from table into a file in the folder 'lists'
### named for the category

### step 3: copy and paste names of major categories and each of the associated minor categories into excel
### spreadsheet, save as csv (some editing afterwards: removed ", and commas in names of categories)

categories <- list.files('lists/')

out <- NULL

for(cc in categories){
    ff <- readLines(paste0('lists/',cc))
    numbers <- grep('\\d',ff,value=TRUE) ## pulls out all the numeric indicators
    out <- rbind(out,cbind(rep(cc,length(numbers)),numbers))
}

out <- as.data.frame(out)
names(out) <- c('small','num')

bigcat <- read.table('bigcat.txt',header=TRUE)

stopifnot(setequal(bigcat$small,categories))

out <- merge(out,bigcat)

write.csv(out,'fodCategories.csv',row.names=FALSE)
