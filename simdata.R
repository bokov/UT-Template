#' ---
#' title: "Simulate Data to Create Demo Dataset"
#' author: 
#' - "Alex F. Bokov^[UT Health, Department of Epidemiology and Biostatistics]"
#' date: "09/14/2018"
#' ---
#' 
#+ message=F,echo=F
# init ----
debug <- 1;
.deps <- c( 'data.R' ); 
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.currentscript <- current_scriptname('simdata.R');
#### simulate data ####
outputsims <- setNames(file.path('data',basename(inputdata)),names(inputdata));
#' Don't simulate files that are remote internet links
outputsims <- outputsims[!grepl('^(ftp|https?)://',outputsims)];
#' Don't simulate files that already exist in the shared directory
outputsims <- outputsims[!file.exists(normalizePath(file.path(.workdir
                                                              ,outputsims)
                                                    ,mustWork = FALSE))];
#' Patch for a format that is only supported for import, not export
outputsims <- gsub('\\.xls$','.xlsx',ignore.case = TRUE,outputsims);

#' Save out the simulated (and thus shareable) versions of the above files to
#' the `data` directory in the same format as the originals where possible
#' and then also save out the newly created data for debug purposes
simrawdata <- list();
if(length(outputsims)>0){
  for(ii in names(outputsims)){
    export(simrawdata[[ii]] <- simdata(rawdata[[ii]])
           ,normalizePath(file.path(.workdir,outputsims[[ii]])
                          ,mustWork = FALSE))};
  if(file.exists('snippets.R')) source('snippets.R') else{
    source(normalizePath(file.path('scripts','snippets.R')))};
  inputsimdata <- inputdata;
  inputsimdata[names(outputsims)] <- outputsims;
  newconfigr <- filesections(normalizePath(file.path(.workdir,'config.R')));
  newconfigr$inputdata <- paste0(.snippets$config_inputdata
                                 ,'c(\n  ',paste(sprintf("%s = '%s'"
                                                         ,names(inputsimdata)
                                                         ,inputsimdata)
                                                 ,collapse='\n ,'),'\n);');
  writeLines(unlist(newconfigr),normalizePath(file.path(.workdir,'config.R')
                                              ,mustWork = FALSE));
}

#+ echo=FALSE
#### save out ####
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
suppressWarnings(save(file=file.path(.workdir,paste0(basename(.currentscript)
                                                   ,'.rdata'))
                      ,list=setdiff(ls(),c('ii','inputsimdata',.origfiles))));
#+ echo=F,eval=F
c()
