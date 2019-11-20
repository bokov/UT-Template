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
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
#+ echo=FALSE,message=FALSE
# read dat00 ----
#' generic read function which auto-guesses file formats:
message('About to autoread');
rawdata <- sapply(inputdata,try_import,simplify=FALSE);

# save out ----
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
suppressWarnings(save(file=file.path(.workdir
                                      ,paste0(basename(.currentscript)
                                              ,'.rdata'))
                       ,list=setdiff(ls(),.origfiles)));
#+ echo=F,eval=F
c()