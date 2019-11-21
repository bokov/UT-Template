#' ---
#' title: "Build Data Dictionary"
#' author: 
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#' 
#+ message=F,echo=F
# init ----
debug <- 0;
.deps <- c( 'data.R' ); 
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
#+ echo=FALSE
#### rename_vars ####
#' Rename variables
#+ rename_vars
map0 <- try_import('varmap.csv');
for(ii in names(rawdata)){
  map0ii <- subset(map0,table==ii);
  if(nrow(map0ii)>0){
    names(rawdata[[ii]]) <- submulti(names(rawdata[[ii]])
                                     ,map0ii[,c('origname','varname')]
                                     ,'startsends')};
};
#+ echo=FALSE
#### data_dct ####
#' Create data dictionaries for all data frames
#+ data_dct
for(ii in names(rawdata)) attr(rawdata[[ii]],'dct') <- tblinfo(rawdata[[ii]]);
#+ echo=FALSE
#### save out ####
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
suppressWarnings(with(rawdata
                      ,save(file=file.path(.workdir
                                           ,paste0(basename(.currentscript)
                                                   ,'.rdata')),list=ls())));
#+ echo=F,eval=F
c()