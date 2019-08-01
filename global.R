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
if(!exists('.workdir')) .workdir <- '.';
#' Upstream repo
options('git.upstream','git@github.com:bokov/2019-SP-TSCI-5050');
#' get current working directory
cwd <- getwd(); cwd;
#' If `global.R` isn't found, try to find it
if(!file.exists('functions.R')){
  cwd <- getwd(); start <- normalizePath('..');
  .corefiles <- '(functions|global|dictionary|example_config).R';
  .candidatedirs <- unique(dirname(file.path(normalizePath('..')
                                             ,list.files(normalizePath('..')
                                                         ,pattern=.corefiles
                                                         ,recursive=T))));
  if(length(.candidatedirs)==0) stop('You are missing required files. '
                                     ,'If this is part of a course, please ask '
                                     ,'your instructor for help. Either way, '
                                     ,'you might need to clone a fresh copy of '
                                     ,'this project.');
  if(length(.candidatedirs)==1){
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
#+ echo=F
# local_functionas ----
#' ## Load some local functions
#+ warning=FALSE, message=FALSE
source('./functions.R');
# Now that we are managing trailR as a standalone package, need devtools
instrequire('devtools');
devtools::install_github('bokov/trailR',ref='integration'); library(trailR);
#+ echo=F
# libs -------------------------------------------------------------------------
#' ## Libraries
#' 
#' load and if necessary install needed libraries
#+ warning=FALSE, message=FALSE
instrequire(
  c(# just-in-time compilation
    # 'compiler'
    
    # propensity scores and glm residuals
    #,'MatchIt','DHARMa'
    
    # zero-inflated poisson, sigh
    #,'pscl'
    
    # various analysis methods
    'survival' # this one can be moved to exploration.R
    #,'MASS','Hmisc','zoo','coin'
    
    # evaluating predictive power
    #,'survAUC','survivalROC','pROC'
    
    # for pd matrices needed by faker()
    #,'Matrix'
    
    # data manipulation & piping. 
    # 'tools' is used by trailR.R
    # 'LaF' is used for fast and powerful reading of text files.
    ,'readr','dplyr','magrittr','tools','LaF' ,'openxlsx'
    ,'tibble','readxl','data.table'
    # dummies breaks categoric variables into individual dummy variables
    ,'dummies'
    #,'lubridate'
    
    # plotting
    ,'ggfortify','survminer'
    #,'ggplot2','grid','GGally','heatmap3','gridExtra','scales'
    
    # string manipulation
    ,'stringi' #,'stringr'
    
    # table formatting
    ,'pander','tableone','broom'
    #,'knitr','htmltab','stargazer','janitor'
    
    # Web
    ,'RCurl','XML'
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
if(!file.exists('config.R')){
  stop('Please copy example_config.R to config.R, modify it so that the '
       ,'\'inputdata\' variable is the full path to your data file on your '
       ,'local computer, back up your config.R to some local location outside '
       ,'this repository, and then try running this script again.')};

source('./config.R');
#' Arguments to any/all file reading expressions (in addition to whatever
#' is already done in config.R)
file_args$skip <- n_skip;
#+ echo=F
# vars -------------------------------------------------------------------------
#' ## Set generic variables
#' 
#' That is to say, variables which can be set without reference to the data and
#' do not take a lot of time to do.
#' 
#' data dictionary template-- metadata that should persist accross multiple 
#' versions of the data and data dictionary
dctfile_tpl <- 'datadictionary_static.csv';
#' checked-in file, with added rows and columns, ready-to-use FOR THIS DATASET
#' if it doesn't exist, it will get created in data.R
dctfile <- paste0('dct_',basename(inputdata));
#' This is the file that lists levels of discrete variables and what each listed
#' level should be renamed to.
levels_map_file <- 'levels_map.csv';
#' random seed
project_seed <- 20190108;
options(gitstamp_prod=F);
#' patient and encounter numbers (you won't necessarily have these in your data)
#' If your data has a patient number and that column is not named `patient_num` 
#' change it here as appropriate.
pn <- 'patient_num';
vn <- 'encounter_num';

#' ## Optional: Wrap the readr functions for audit trail
spec_delim <- function(xx,...) tread(xx,readr::spec_delim,...);
read_delim <- function(xx,...) tread(xx,readr::read_delim,...);

#+ echo=F
# searchrep --------------------------------------------------------------------
#' Certain data has text that you will always want to remove wherever it's.
#' This is the place for it. You can leave the current value as a placeholder
#' for now because it's unlikely to show up in your own dataset.
globalsearchrep <- rbind(
  c('\\[[0-9,]+ facts; [0-9,]+ patients\\]','')
);

#+ echo=F
# urls -------------------------------------------------------------------------
urls <- list(
  # recent version of compiled document online
  # (not relevant to TSCI 5050 except as an example)
   exp_rpubs='https://rpubs.com/bokov/kidneycancer'
  # NAACCR data dictionary, section 10
  ,dict_naaccr='http://datadictionary.naaccr.org/?c=10'
  # TSCI 5050 website
  ,git_site='https://github.com/bokov/2019-SP-TSCI-5050'
  );
#' RPubs keeps the actual content in an iframe, and this cool little 3-liner 
#' gets the address of that iframe's target so in principle I can now construct
#' links with targets to other documents I published previously, starting with
#' the most recent version of this document.
urls$exp_raw <- getURL(urls$exp_rpubs) %>% 
  htmlParse %>% xpathApply('//iframe') %>% `[[`(1) %>% xmlAttrs() %>% 
  paste0('https:',.);
#+ echo=F
# fs_templates -----------------------------------------------------------------
#' templates for `fs()` ... note that the actual values inserted depend on 
#' the other arguments of `fs()` and the columns of the data dictionary
fstmplts <- list(
  # [n_ddiag]: #n_ddiag "0390 Date of Diagnosis"
  linkref="[%1$s]: %2$s \"%4$s\"\n"  
  # [`n_ddiag`][#n_ddiag]
  ,link_varname="[`%1$s`][%2$s]"
  # [`0390 Date of Diagnosis`][#n_ddiag]
  ,link_colnamelong="[`%4$s`][%2$s]"
  # `0390 Date of Diagnosis`
  ,code_colnamelong="`%4$s`"
  # 0390 Date of Diagnosis
  ,plain_colnamelong="%4$s"
  # note2self spans, each linking to a ticket
  ,n2s=paste0('(',urls$git_tix,'%1$s){#gh%1$s .note2self custom-style="note2self"}')
  # NCDB style variable definitions
  ,ncdb_def=paste(':::::{%2$s .vardef custom-style=\"vardef\"}',' %4$s :'
                  ,'  ~ %1$s\n\n',sep='\n\n')
);
#+ echo=F,eval=F
c()
