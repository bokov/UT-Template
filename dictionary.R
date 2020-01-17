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
.currentscript <- current_scriptname('dictionary.R');
#' Saving original file-list so we don't keep exporting functions and
#' environment variables to other scripts
.origfiles <- ls();
#+ echo=FALSE
#### rename_vars ####
#' Rename variables
#+ rename_vars
map0 <- try_import(file.path(.workdir,'varmap.csv'));
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
for(ii in names(rawdata)) attr(rawdata[[ii]],'tblinfo') <- tblinfo(rawdata[[ii]]);
.updaterawdata <- FALSE;
#' If necessary, create demonstration variables so the scripts that need
#' some kind of predictors and outcomes don't crash the first time they run
if(length(v(c_safetf,rawdata[[1]]))==0){
  .updaterawdata <- TRUE;
  rawdata[[1]]$EXAMPLEVBIN01 <- with_cm(sample(c('A','B')
                                               ,nrow(rawdata[[1]]),rep=TRUE)
                                        ,'Simulated variable added, for demo.')};
if(length(v(c_safenumeric,rawdata[[1]]))==0){
  .updaterawdata <- TRUE;
  rawdata[[1]]$EXAMPLEVNUM02 <- with_cm(rnorm(nrow(rawdata[[1]]))
                                        ,'Simulated variable added, for demo.')};
if(length(setdiff(v(c_safe,rawdata[[1]]),c(v(c_safetf,rawdata[[1]])
                                           ,v(c_safenum,rawdata[[1]]))))==0){
  .updaterawdata <- TRUE;
  rawdata[[1]]$EXAMPLEVBIN03 <- with_cm(sample(c('A','B'),nrow(rawdata[[1]])
                                               ,rep=TRUE)
                                        ,'Simulated variable added, for demo.');
  rawdata[[1]]$EXAMPLEVNUM04 <- with_cm(rnorm(nrow(rawdata[[1]]))
                                        ,'Simulated variable added, for demo.');
}
if(.updaterawdata) attr(rawdata[[1]],'tblinfo') <- tblinfo(rawdata[[1]]);
#+ echo=FALSE
#### save out ####
#' ## Save all the processed data to an rdata file
#'
suppressWarnings(with(rawdata
                      ,save(file=file.path(.workdir
                                           ,paste0(basename(.currentscript)
                                                   ,'.rdata'))
                            ,list=c(ls(),'map0'))));
#+ echo=F,eval=F
c()
