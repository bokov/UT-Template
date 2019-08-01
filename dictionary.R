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
# read dat0 ----
#' generic read function which auto-guesses file formats:
dat0 <- t_autoread(inputdata,file_args=file_args);

#' ## Optional: patient number
#' 
#' If you patient number variable (see `global.R`) is a number, force it to be
#' treated as character rather than an integer to avoid missing values due to it 
#' being too large
if(pn %in% names(dat0)) dat0[[pn]] <- as.character(dat0[[pn]]);

#+ echo=F
# make data dictionary ----
#' ## Create the data dictionary
dct0 <- tblinfo(dat0);

#+ echo=F
# a few dat0 hacks ----
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