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
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();
# read student pre-run script if it exists ----
if('pre_dictionary.R' %in% list.files()) source('pre_dictionary.R');

#+ echo=F
# read dat00 ----
#' generic read function which auto-guesses file formats:
message('About to autoread');
dat00 <- t_autoread(inputdata,file_args=file_args);
message('Done autoread');

#+ echo=F
# make data dictionary ----
#' ## Create the data dictionary
dct0 <- tblinfo(dat00);

#+ echo=F
# a few dat00 hacks ----
#' ## Raw Data Ops
#' 
#' Since you're messing around with the raw data anyway, if there is anything 
#' you will need later which does not depend on the processing steps in the
#' `data.R` script, you might as well get it out of the way in this section

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