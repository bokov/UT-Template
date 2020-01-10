#' ---
#' title: "Read in Data"
#' author: 
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#' 
#+ message=F,echo=F
# init ----
debug <- 0;
.projpackages <- c('dplyr');
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
#.currentscript <- parent.frame(2)$ofile;
.currentscript <- current_scriptname('data.R');
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
#+ echo=FALSE,message=FALSE
# read data ----
#' generic read function which auto-guesses file formats:
message('About to autoread');

rawdata <- sapply(inputdata,try_import,simplify=FALSE);
# rename columns
if(!file.exists(file.path(.workdir,'varmap.csv'))){
  map0 <- sapply(rawdata,makevarmap,simplify=FALSE) %>% c(.id='table') %>%
    do.call(bind_rows,.);
  write.csv(map0,file.path(.workdir,'varmap.csv'),row.names = FALSE);
} else {
  map0 <- try_import(file.path(.workdir,'varmap.csv'));
}

# save out ----
#' ## Save all the processed data to an rdata file 
#' 
suppressWarnings(save(file=file.path(.workdir
                                      ,paste0(basename(.currentscript)
                                              ,'.rdata'))
                       ,list=setdiff(ls(),.origfiles)));
#+ echo=F,eval=F
c()
