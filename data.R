#' ---
#' title: "Generic Data Processing Script"
#' author: "Alex F. Bokov"
#' date: "08/15/2017"
#' ---
#' 
#+ message=F,echo=F
# init -------------------------------------------------------------------------
debug <- 0;
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.depends <- 'dictionary.R';
#.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
#if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
#.loadedobjects <- tload(.depdata);
.loadedobjects <- load_deps(.depends,cachedir = .workdir);
#knitr::opts_chunk$set(echo = F,warning = F,message=F);
#' Default args (example)
#formals(v)$dat <- as.name('dat1');

#' Saving original file-list so we don't keep exporting functions and 
#' environment variables to other scripts
.origfiles <- ls();

# read student pre-run script if it exists ----
if('pre_data.R' %in% list.files()) source('pre_data.R');

#' Create custom synonyms for `TRUE` if needed
l_truthy_default <- eval(formals(truthy.default)$truewords);
l_missing <- c(NA,'Unknown','unknown','UNKNOWN');
# dat1 organize codes ----------------------------------------------------------
#' Load manual code mappings
if(file.exists(levels_map_file)){
  levels_map <- tread(levels_map_file,read_csv,na='')};

#' Create copy of original dataset
if(pn %in% names(dat0)){
  groupvar <- pn;
}

if(exists('groupvar') && groupvar %in% names(dat0)){
  dat1 <- group_by(dat0,patient_num);
} else dat1 <- dat0;
#' 
#' Bulk-transform the NA/non-NA columns to FALSE/TRUE ones
#
#' 
#' Rename columns that will be individually referenced later on so that they
#
#' 
#' always have the same name regardless of the data version
#
#' 
#' Mass relabel/reorder factor variables.
#
#' 
#' Create binned versions of certain numeric vars.
#
#' 
#' Deal with sets of columns that need to be combined with each other 

#+ echo=F
# subsets  ---------------------------------------------------------------------
#' ## Subsets
#' 
#' You might have groups of subjects or groups of observations in your data that
#' will later be analyzed separately. This is a good place ot create lists of 
#' their IDs. Example: unique patient numbers for patients who happen to have
#' `TRUE` as the value of either `var1` or `var2` 
#kcpatients.emr <- subset(dat1,var1|var2)$patient_num %>% unique;

#+ echo=F
# dat1 more analytical variables  ----------------------------------------------
#' ## More Analytical Variables
#' 
#' Any other within-row transformations you need to do on your data?

#+ echo=F
# temporal variables for longitudinal/repeated-measures/survival data ----------
#' ## Mass-converting variables to time-to-event form
#' 
#' If you have time-to-event data, this is the place for transforming it from
#' (for example)
#+ echo=F
c(NA,NA,NA,NA,'EVENT',NA,NA);
#' ...to (for example)
#+ echo=F
c(-320,-241,-116,-15,1,46,89)
#' 
#' And other transformations that depend not only on one row, but an entire 
#' group of rows corresponding to the same patient or test subject

#+ echo=F
# more analytic variable tweaks ------------------------------------------------
#' ## Final Data Transformations
#' 
#' Any transformations that have to be done after longitudinal transformations
#' in the above section. You should not do any transformations of dat1 below
#' this section of the script

#+ echo=F
# training/testing/validation samples ------------------------------------------
#' ## Creating training/testing/validation samples
#' 
#' As long as the seed is the same, all random values will be generated the same
#' reproducibly.
tseed(project_seed);
#' Randomly to training, testing, or validation sets
if(pn %in% names(dat1)) pat_samples <- unique(dat1[[pn]]) %>% 
    split(.,sample(c('train','test','val'),size=length(.),rep=T));

#+ echo=F
# dat2, one-per-patient --------------------------------------------------------
#' ## Create a version of the dataset that only has each patient's 1st encounter
#' 
#' (if applicable)

# read student post-run script if it exists ----
if('post_data.R' %in% list.files()) source('post_data.R');

#+ echo=F
# save out ---------------------------------------------------------------------
#' ## Save all the processed data to an rdata file 
#' 
#' ...which includes the audit trail
tsave(file=file.path(.workdir,paste0(basename(.currentscript),'.rdata'))
      ,list=setdiff(ls(),c(.origfiles)));
#+ echo=F,eval=F
c()