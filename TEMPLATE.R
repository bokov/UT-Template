#' ---
#' title: "%1$s"
#' author: "%2$s"
#' date: "%3$s"
#' ---
#' 
#+ message=F,echo=F
# init -----

# set to > 0 for verbose initialization
debug <- 0;

# vector of additional packages to install, if needed. If none needed, should be
# an empty string
.packages <- %4$s;

# name of this script
.currentscript <- "%5$s"; 

# vector of other scripts which need to run before this one. If none needed, 
# should be an empty string
.deps <- %6$s; 

# load stuff ----
# load project-wide settings, including your personalized config.R
if(debug>0) source('%7$s',chdir=T) else {
  .junk<-capture.output(source('%7$s',chdir=T,echo=F))};
# load any additional packages needed by just this script
if(length(.packages) > 1 || .packages != '') instrequire(.packages);
# start logging
tself(scriptname=.currentscript);
# Use the workdir
.workdir <- getwd();
# run scripts on which this one depends, if any that have not been cached yet
.loadedobjects <- load_deps(.deps,cachedir = .workdir);

# which files are here before anything new is created by this script
.origfiles <- ls(all=T);

#+ echo=F
#############################################################
# Your code goes below, content provided only as an example #
#############################################################

#' ### Data Dictionary
#' 
#' Quality control, descriptive statistics, etc.

#+ echo=F
# characterization ----
set.caption('Data Dictionary');
set.alignment(row.names='right')
.oldopt00 <- panderOptions('table.continues');
panderOptions('table.continues','Data Dictionary (continued)');
#  render the Data Dictionary table
pander(dct0[,-1],row.names=dct0$column,split.tables=Inf); 
#  reset this option to its previous value
panderOptions('table.continues',.oldopt00);

#' ### Select predictor and outcome variables (step 8)
#' 
#' Predictors
# Uncomment the below line after putting in the actual predictor column names
# from your dat0
#predictorvars <- c('FOO','BAR','BAZ','BAT');
#' Outcomes
# Uncomment the below line after putting in the actual outcome column names
# from your dat0
#outcomevars <- c('BAN','BAX');
#' All analysis-ready variables
# Uncomment the below line after predictorvars and outcomevars already exist
#mainvars <- c(outcomevars, predictorvars);
#' ### Scatterplot matrix (step 10)
#' 
#' To explore pairwise relationships between all variables of interest.
#+ ggpairs_plot
# Uncomment the below after mainvars already exists and you have chosen a 
# discrete variable to take the place of VAR1 (note that you don't quote that
# one)
#ggpairs(dat0[,mainvars],mapping=aes(color=VAR1));
#' ### Cohort Characterization (step 10)
#' 
#' To explore possible covariates
# Uncomment the below code after mainvars exists and you have chosen a discrete
# variable to take the place of VAR1 (this time you do quote it)
#
#pander(print(CreateTableOne(
#  vars = setdiff(mainvars,'VAR1'),strata='VAR1',data = dat0
#  , includeNA = T), printToggle=F), caption='Group Characterization');

#' ### Data Analysis
#' 
#' Fitting the actual statistical models.
#+ echo=F
# analysis ----

#+ echo=F
#############################################################
# End of your code, start of boilerplate code               #
#############################################################

# save out with audit trail ----
# Saving original file-list so we don't keep exporting functions and 
# environment variables to other scripts. Could also replace .origfiles
# with the expression c(.origfiles,.loadedobjects) and then, nothing
# get inherited from previous files, only the stuff created in this 
# script gets saved. If this doesn't make sense, don't worry about it.
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles)
      ,verbose=F);

#' ### Audit Trail
#+ echo=F
.wt <- walktrail();
pander(.wt[order(.wt$sequence),-5],split.tables=Inf,justify='left',missing=''
       ,row.names=F);
#+ echo=F,eval=F
c()
