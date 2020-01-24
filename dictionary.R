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
.projpackages <- c( 'purrr' );
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

#+ samples_specify
# samples_specify ----
#' Create samples
set.seed(project_seed);
#' This is the format for specifying samples. The two default/required sampling
#' strategies are `subsample`, which creates mutually exclusive subsamples whose
#' number and sizes are determined by the prob argument, and `resample` which
#' resamples with replacement a number of times determined by the `nn` argument.
#' If you define a `samples_specify` list in your `config.R` in the same format
#' as below, then your values for `prob` and `nn` will be used (and you can
#' leave either out, which will cause the appropriate below default to be used
#' in place of the missing value. In addition you can specify other sampling
#' strategies by adding list items. The name of the item should be the 
#' name of a function that takes a vector as its first argument and whose 
#' additional arguments are set in the list that is assigned to it. The function
#' must return a named list of vectors based on the input vector.
#' For example `subsample()` is a function that takes a vector as its first 
#' argument, `prob` as its second argument, and returns a list of vectors each
#' of which is a subset of the original.
#' 
#' Insure missing required values are filled from defaults but preserve any
#' that have been already specified by the analyst

.sampdefaults <- list(subsample=list(prob=c(.5,.5)),resample=list(nn=5));

if(!exists('samples_specify')) samples_specify <- .sampdefaults;
if(is.null(samples_specify$subsample)){
  samples_specify$subsample <- .sampdefaults$subsample};
if(is.null(samples_specify$subsample$prob)){
  samples_specify$subsample$prob <- .sampdefaults$subsample$prob};
if(is.null(samples_specify$resample)){
  samples_specify$resample <- .sampdefaults$resample};
if(is.null(samples_specify$subsample$prob)){
  samples_specify$subsample$prob <- .sampdefaults$subsample$prob};
#' Create the actual sampling indexes for all data.frames
#' 
samples_of_data<-lapply(rawdata,function(xx){
  sapply(names(samples_specify),function(yy){
      do.call(yy,c(list(seq_len(nrow(xx))),samples_specify[[yy]]))}) %>% 
    flatten;
  })


#' Now, we specify which datasets will use which of the generated sampling 
#' indexes. We insure it exists and fill in missing required values similarly
#' to `samples_specify` except now it's just a named vector. The names are the
#' names of data.frames and the values are which sampling index to use for those
#' data.frames
#+ samples_use
# samples_use ----
if(!exists('samples_use')){
  samples_use <- setNames('',names(rawdata)[1]);
} else {
  samples_use <- samples_use[intersect(names(samples_use),names(rawdata))];
  if(length(samples_use)==0) samples_use <- setNames('',names(rawdata)[1]);
};
samples_use <- tolower(samples_use);

#' Save out the whole data, before replacing some of it with samples 
suppressWarnings(save(file=file.path(.workdir
                                     ,paste0(basename(.currentscript)
                                             ,'.unsampled.rdata'))
                      ,list=c('rawdata','samples_specify','samples_use'
                              ,'samples_of_data','map0')));
#' Do the sampling
#+ do_sampling
# do_sampling ----
for(ii in names(samples_use)){
  # 'all' is a special sample name that means don't sample, just use the 
  # original data
  iidx <- samples_use[ii];
  if(iidx=='all') next();
  if(!iidx %in% names(samples_of_data[[ii]])){
    warning('The ',iidx,' sample index requested for ',ii, ' is not available');
    if(length(samples_of_data[[ii]][['subsample001']])>100){
      message('Falling back on subsample001 for ',ii);
      samples_use[ii] <- iidx <- 'subsample001' } 
    else if(length(samples_of_data[[ii]][['resample001']])>2){
      message('Falling back on resample001 for ',ii);
      samples_use[ii] <- iidx <- 'resample001' } 
    else {
      samples_use[ii] <- iidx <- names(samples_of_data[[ii]][1]);
      message('Falling back on ',iidx,' for ',ii);
    }
  };
  iitblinfo <- attr(rawdata[[ii]],'tblinfo');
  rawdata[[ii]] <- rawdata[[ii]][samples_of_data[[ii]][[iidx]],];
  attr(rawdata[[ii]],'tblinfo') <- iitblinfo;
}
#' 
#' Create the samples

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
