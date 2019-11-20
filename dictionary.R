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
# read student pre-run script if it exists ----
#+ echo=FALSE,message=FALSE
# read dat00 ----
#' generic read function which auto-guesses file formats:
#+ echo=F
# make data dictionary ----
#' ## Create the data dictionary
dct0 <- tblinfo(dat00);
message('Done creating data dictionary, starting variable mapping');
#' The `varmap.csv` file is for renaming variables. Create
#' a 'no-change' starter version if one doesn't already exist
if(!file.exists(file.path(.workdir,'varmap.csv'))){
  map0 <- tibble::as_tibble(with(dct0,data.frame(origname=column,varname=column
                                         ,dispname=column,comments=NA
                                         ,stringsAsFactors = FALSE)));
  write.csv(map0,file.path(.workdir,'varmap.csv'),row.names = FALSE);
} else {
  map0 <- try_import(file.path(.workdir,'varmap.csv'));
}
dct0$column <- make.unique(unlist(submulti(dct0$column,map0
                                           ,method='startsends')));
names(dat00) <-dct0$column;
message('Done variable mapping');
#+ echo=F
# save out ----
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
suppressWarnings(tsave(file=file.path(.workdir
                                      ,paste0(basename(.currentscript)
                                              ,'.rdata'))
                       ,list=setdiff(ls(),.origfiles)));
#+ echo=F,eval=F
c()