#' ---
#' title: "Generic Global R Settings"
#' author: "Alex F. Bokov"
#' date: "10/18/2018"
#' ---
#'
#' This is a script that loads (and if needed, installs)
#' libraries needed for this project, and sets a few global
#' variables that will be needed by many different scripts.
#' Keep this script minimalistic and *no number crunching here*
#'
#+ echo=F
# orient_paths ----
#' ## Figure out where we are and set the upstream repository
#'
#' Upstream repo
options(ripcord.startrun=getOption('ripcord.startrun',Sys.time()));
options(ripcord.messagefun=getOption('ripcord.messagefun'
                                     ,if(isNamespaceLoaded('cli')){
                                       function(xx,...){
                                         message(cli::col_grey(xx,...))}
                                     } else message));
options(git.upstream='git@github.com:bokov/2019-FA-TSCI-5050');
options(repos=c(CRAN='https://cloud.r-project.org'));
options(datatable.na.strings = getOption('datatable.na.strings'
                                         ,c('NA','','.','-','(null)','NULL'
                                            ,'N/A','null')));
options(datatable.integer64=getOption('datatable.integer64','character'));

#' get current working directory
cwd <- getwd(); cwd;
#' If `global.R` isn't found, try to find it
if(!file.exists('functions.R')){
  message('Attempting to track down global.R file...');
  start <- normalizePath('..',winslash='/');
  .corefiles <- '(functions|global|dictionary|example_config).R';
  .candidatedirs <- unique(dirname(file.path(normalizePath('..',winslash='/')
                                             ,list.files(normalizePath('..',winslash='/')
                                                         ,pattern=.corefiles
                                                         ,recursive=T))));
  if(length(.candidatedirs)==0) stop('You are missing required files. '
                                     ,'If this is part of a course, please ask '
                                     ,'your instructor for help. Either way, '
                                     ,'you might need to clone a fresh copy of '
                                     ,'this project.');
  if(length(.candidatedirs)>1){
    message('You are not in the correct directory. Here is/are one/s that '
            ,'might be correct. Please type in the directory to which you wish '
            ,'to switch to or hit [enter] to accept the first item on this '
            ,'list:');
    cat(paste(' ',.candidatedirs),sep='\n');
    .response <- readline('Choose directory: ');
    if(.response=='') setwd(.candidatedirs[1]) else {
      if(!dir.exists(.response)) stop('The directory you chose, '
                                      ,.response,' does not exist. Try again.');
      setwd(.response)};
  }
}
#' ## Set variables that may vary from one script to another, if they are
#' not already set in the calling script
if(!exists('.debug')) .debug <- 0;
if(!exists('.projpackages')) .projpackages <- c('');
if(!exists('.currentscript')) .currentscript <- 'UNKNOWN_SCRIPT';
if(!exists('.deps')) .deps <- c('');

#
if(!require('devtools',quietly=.debug==0)){
  install.packages('devtools',dependencies=TRUE,quiet=.debug==0
                   ,repos=getOption('repos','https://cran.rstudio.com'))};
#devtools::install_github('bokov/trailR',ref='integration'); library(trailR);
devtools::install_github('bokov/tidbits',ref='integration'
                         ,quiet= .debug == 0);
devtools::install_github('bokov/rio',ref='master'
                         ,quiet= .debug == 0);
library(rio,quietly= .debug==0, warn.conflicts = .debug>0, verbose = .debug>0);
rio::install_formats(quiet=.debug==0
                     ,repos=getOption('repos','https://cran.rstudio.com'))
library(tidbits,quietly= .debug==0, warn.conflicts = .debug>0, verbose = .debug>0);



#+ echo=F
# local_functions ----
#' ## Load some local functions
#+ warning=FALSE, message=FALSE
if(file.exists('functions.R')) source('functions.R');

#+ echo=F
# libs -------------------------------------------------------------------------
#' ## Libraries
#'
#' load and if necessary install needed libraries
#+ warning=FALSE, message=FALSE
instrequire(
  c(# argument validation
    'checkmate'
    # text formatting
    ,'cli'
    # just-in-time compilation
    # 'compiler'

    # propensity scores and glm residuals
    #,'MatchIt','DHARMa'

    # zero-inflated poisson, sigh
    #,'pscl'

    # various analysis methods
    #'survival'
    #,'MASS','Hmisc','zoo','coin'

    # evaluating predictive power
    #,'survAUC','survivalROC','pROC'

    # for pd matrices needed by faker()
    #,'Matrix'

    # data manipulation & piping.
    # 'tools' was used by trailR.R
    # 'LaF' is used for fast and powerful reading of text files.
    #,'readr','dplyr','LaF','tools','openxlsx'
    #,'magrittr'
    #,'tibble','readxl','data.table','haven'
    # dummies breaks categoric variables into individual dummy variables
    #,'dummies'
    #,'lubridate'

    # plotting
    #,'ggfortify','survminer'
    #,'ggplot2','grid','GGally','heatmap3','gridExtra','scales'

    # string manipulation
    #,'stringi' #,'stringr'

    # table formatting
    #,'pander','tableone','broom'
    #,'knitr','htmltab','stargazer','janitor'

    # Web
    #,'RCurl','XML'
));

#' Turn JIT to max: pre-compile all closures, `for`, `while`, and `repeat` loops
#' (not needed unless you're running some very slow operations)
#enableJIT(3);
#+ echo=F
# config ----
#' ## Set variables that can get overridden by `config.R` if
#' applicable (to avoid error messages if you don't have them in
#' your `config.R`)
n_skip <- 0;
file_args <- list(check.names=T,blank.lines.skip=T);
#' ## Load local config file
#'
# local_config ----
.configpath <- tidbits::find_filepath('config.R'
                                      ,pathexcl='[[:punct:]]backup\\.');
if(is.null(.configpath)){
  stop('Please copy example_config.R to config.R, modify it so that the '
       ,'\'inputdata\' variable is the full path to your data file on your '
       ,'local computer, back up your config.R to some local location outside '
       ,'this repository, and then try running this script again.')};

if(!exists('.workdir')) .workdir <- dirname(.configpath);

source(.configpath);
.configlocal <- file.path(dirname(.configpath),'local.config.R');
if(file.exists(.configlocal)) source(.configlocal);
#' Validata the inputdata variable
if(!exists('inputdata')) stop('Your `config.R` or `local.config.R` file does '
                              ,'not have an `inputdata` variable set');
#' Make sure all files listed in `inputdata` actually exist
for(ii in seq_along(inputdata)){
  if(is.null(.iipath<-tidbits::find_filepath(inputdata[ii]))){
    stop('Cannot find file ',inputdata[ii])} else {
      inputdata[ii] <- .iipath}};
inputdata <- setNames(normalizePath(inputdata,winslash='/'),names(inputdata));
if(.debug) message('Inputdata:\n\t',paste(inputdata,collapse=',\n\t'));
#' Arguments to any/all file reading expressions (in addition to whatever
#' is already done in config.R)
file_args$skip <- n_skip;
#+ echo=F
# vars ----
#' ## Set generic variables
#'
#' That is to say, variables which can be set without reference to the data
#' and do not take a lot of time to do.
#'

options(tb.retcol='column');
#' data dictionary template-- metadata that should persist accross multiple
#' versions of the data and data dictionary
dctfile_tpl <- 'datadictionary_static.csv';
#' checked-in file, with added rows and columns, ready-to-use FOR THIS DATASET
#' if it doesn't exist, it will get created in data.R
dctfile <- paste0('dct_',basename(inputdata));
#' This is the file that lists levels of discrete variables and what each
#' listed level should be renamed to.
levels_map_file <- 'levels_map.csv';
#' random seed
project_seed <- 20190108;
options(gitstamp_prod=F);

#+ echo=F
# searchrep ----
#' Certain data has text that you will always want to remove wherever it's.
#' This is the place for it. You can leave the current value as a placeholder
#' for now because it's unlikely to show up in your own dataset.
globalsearchrep <- rbind(
  c('\\[[0-9,]+ facts; [0-9,]+ patients\\]','')
);

#+ echo=FALSE
# urls ----
urls <- list(
  # TSCI 5050 website
  git_site='https://github.com/bokov/2019-FA-TSCI-5050'
  );

#+ echo=FALSE,eval=F
# script-specific packages ----
if(length(setdiff(.projpackages,'') > 0)) if(.debug==0) {
  .junk <- suppress(instrequire(.projpackages));
} else instrequire(.projpackages);
# start logging ----
#if(exists('tself')) tself(scriptname=.currentscript);
# run scripts on which this one depends ----
# if any that have not been cached yet
setwd(.workdir);
#.loadedobjects <- tidbits:::load_deps(.deps,cachedir = .workdir);
if(.debug) message('LOADING DEPENDENCIES');
.loadedobjects <- load_deps2(.deps,cachedir = .workdir,debug=.debug);
if(exists('projectoptions')){
  message('Loading project options');
  options(projectoptions);
}
# files already existing ----
.origfiles <- ls(all=TRUE);
c()
