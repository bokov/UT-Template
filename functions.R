# small utils ------------------------------------------------------------------

#' I'd rather not introduce a dependency for just one function, on the other
#' I trust Dr. Wickam & Co.'s code more than my own inexperienced stumblings so 
#' if coalesce is already available, I use that one.
#'  
if(!exists('coalesce')){
  coalesce <- function(...){
    Reduce(function(xx,yy) ifelse(is.na(xx),yy,xx),list(...))}
}

#' This function takes a list of package named, loads them if they are
#' available, otherwise attempts to install each one and then again 
#' attempts to load it.
instrequire <- function(pkglist
                        ,quietly=TRUE
                        ,repos=getOption('repos','https://cran.rstudio.com/')){
  pkgs_installed <- sapply(pkglist,require,character.only=T);
  if(length(pkgs_needed <- names(pkgs_installed[!pkgs_installed]))>0){
    install.packages(pkgs_needed,repos=repos,dependencies = T);
    pkgs_final <- sapply(pkgs_needed,require,character.only=T,quietly=quietly);
    if(!all(pkgs_final)){
      stop(c('the following required packages could not be installed:\n'
             ,paste0(names(pkgs_final[!pkgs_final]),collapse = ', ')));
    }
  };
}


#' Function for appending or replacing attributes of any object 
#' within a pipeline
#'
#' @param xx        Object whose attributes to modify and then return the object
#' @param rep.attrs Named list of attributes to create or replace
#' @param app.attrs Named list of attributes to create or append to
#'
#' @return The object `xx` with modified attributes
#' @export
#'
#' @examples
with_attrs<-function(xx,rep.attrs=list(),app.attrs=list()){
  attrs <- attributes(xx); if(is.null(attrs)) attrs<-list();
  for(ii in names(rep.attrs)) attrs[[ii]] <- rep.attrs[[ii]];
  for(ii in names(app.attrs)) attrs[[ii]] <- c(attrs[[ii]],app.attrs[[ii]]);
  attributes(xx) <- attrs;
  xx;
}

#' takes input and returns it with a comment attribute
cm <- with_cm <- function(xx,comment=NULL,append=T
                          ,transf=stringr::str_squish){
  if(!is.null(transf)) comment <- transf(comment);
  if(append) with_attrs(xx,app.attrs=list(comment=comment)) else {
    with_attrs(xx,rep.attrs=list(comment=comment));
  }
}

#' Take an object name \code{obj}, check to see if it  exists in environment \code{env}
#' and if it does not, run \code{expression} \code{EXPR} and assign its result to \code{obj}
#'
#' @param obj   A \code{character} string (required) naming the variable in env
#'   to which the results of running \code{EXPR} will be assigned.
#' @param EXPR  An \code{expression} to execute and assign to the object named
#'   by \code{obj} if it doesn't already exist.
#' @param env   An \code{environment} to check for the existence of \code{obj}
#'   and where to create it if it doesn't exist.
#'
#' @return Does not return anything.
#'
#' @examples `checkrun('dat3',{group_by(dat1,idn_mrn) %>% summarise_all(first)});`
checkrun <- function(obj,EXPR,env=as.environment(-1)){
  env<-env;
  if(length(ls(env,all=T,pattern=obj))==0){
    browser();
  }
}

getCall.list <- getCall.data.frame <- getCall.gg <- function(xx) {attr(xx,'call')};

# why not update calls?
update.call <- function(xx,...){
  dots <- list(...);
  for(ii in names(dots)) xx[[ii]] <- dots[[ii]];
  xx;
}

#' Stack a vector to form a matrix with repeating rows, with optional 
#' column names and transformation
#'
#' @param  vv    A vector which will become the row of a matrix
#' @param  nr    Number of (identical) rows this matrix will contain
#' @param  trans An optional function that can take a matrix as its 
#'              sole argument. Useful functions include `as.data.frame()`
#'              `as_tibble()` and `as.table()`
#' @return A matrix, unless the function specified in the `trans` argument
#'         causes the output to be something else.
#' @export 
#'
#' @examples 
#' vec2m(1:10,5);
#' vec2m(1:10,5,tr=data.frame);
#' vec2m(setNames(1:12,month.name),3);
vec2m <- function(vv,nr=1,trans=identity) {
  return(trans(matrix(as.matrix(c(vv)),nrow=nr,ncol=length(vv),byrow=T
                      ,dimnames=list(NULL,names(vv)))));
}

#' returns call with ALL arguments specified, both the defaults and those
#' explicitly provided by the user
fullargs <- function(syspar=sys.parent(),env=parent.frame(2L),expand.dots=TRUE){
  fn <- sys.function(syspar);
  frm <- formals(fn);
  cll <- match.call(fn,sys.call(syspar),expand.dots = expand.dots,envir = env);
  defaults <- setdiff(names(frm),c(names(cll),'...'));
  for(ii in defaults) cll[[ii]] <- frm[[ii]];
  return(cll);
}

#' Take a set of objects coercible to matrices and perform sprintf on them while
#' preserving their dimensions (obtained from the first argument of ...)

# figure out how the current OS represents the top of its file system
systemRootDir <- function(){
  dir <- dirname(normalizePath('.'));
  newdir <- dirname(dir);
  while(dir!=newdir){dir<-newdir; newdir <- dirname(newdir)}
  return(newdir);
}

mprintf <- function(fmt,...,flattenmethod=1){
  dots <- list(...);
  out<-dots[[1]];
  # if factors not converted to characters, those cells will come out as NA
  if(is.factor(out)) out <- as.character(out) else if(is.list(out)){
    for(ii in seq_along(out)) if(is.factor(out[[ii]])) {
      out[[ii]]<-as.character(out[[ii]])}}
  if(is.null(nrow(out))) {
    warning('Converting output to matrix. Might be errors.');
    outnames<-names(out);
    out <- t(matrix(out));
    try(colnames(out)<-outnames);
  }
  for(ii in seq_along(dots)) dots[[ii]] <- c(if(flattenmethod==1) {
    unlist(dots[[ii]])} else if(flattenmethod==2){
      sapply(dots[[ii]],as.character)});
  vals <- do.call(sprintf,c(fmt,dots));
  for(ii in seq_len(nrow(out))) for(jj in seq_len(ncol(out))) {
    out[ii,jj] <-matrix(vals,nrow=nrow(out))[ii,jj]};
  out;
  }

# extract the error message of the argument
getTryMsg <- function(xx,ifNotErr=xx){
  if(is(xx,'try-error')) return(attr(bla,'condition')$message);
  return(ifNotErr);}

# to be used inside a function to get a list of unevaluated calls 
# from all the ... args
getParentDots <- function(xx,call=sys.call(-1),fun=sys.function(-1)){
  out <- list();
  for(ii in setdiff(names(call),c(names(formals(fun)),''))){
    out[[ii]] <- call[[ii]]};
  out;
}

systemwrapper <- function(cmd='',...,VERBOSE=getOption('sysverbose',T)
                          ,CHECKFILES=c('files')){
  args <- list(...); sysargs <- list();
  # separate out the args intended for system
  for(ii in intersect(names(args),names(formals(system)))){
    sysargs[[ii]] <- args[[ii]]; args[[ii]] <- NULL;};
  # check to make sure all arguments listed in checkfiles contain only files
  # that exist
  for(ii in intersect(CHECKFILES,names(args))){
    if(!all(.exist <- file.exists(args[[ii]]))){
      stop('The following files cannot be found:\n'
           ,paste(args[[ii]][!.exist],collapse=', '))}};
  for(xx in args) cmd <- paste(cmd,paste(xx,collapse=' '));
  if(VERBOSE) message('Executing the following command:\n',cmd);
  return(do.call(system,c(command=cmd,sysargs)));
}
# git ----
git_checkout <- function(which=getOption('git.workingbranch','master'),...){
  systemwrapper('git checkout',which,...)};
gco <- git_checkout;

git_commit <- function(files='-a',comment
                       ,autopush=getOption('git.autopush',T),...){
  .changed<-git_status(VERBOSE=F,intern=T);
  filenames <- if(!missing(files)){
    paste0(paste(files,collapse=','),': ')} else 'multi: ';
  comment <- paste0('"',filenames,comment,'"');
  systemwrapper('git commit',files,'-m',comment,...);
  if(autopush) git_push();}
gci <- git_commit;

# List the files in the repo having a particular status
git_diff_filter <- function(xx) {
  system(paste('git diff --name-only --diff-filter',xx),intern=T)};

git_status <- function(print=T
                       ,diff_filters=list(Added='A',Copied='C',Deleted='D'
                                          ,Modified='M',Renamed='R'
                                          ,ChangedType='T',Unmerged='U'
                                          ,Unknown='X',Broken='B')
                       ,...){
  branch <- system('git rev-parse --abbrev-ref HEAD',intern=T);
  tracking <- system('git rev-parse --abbrev-ref --symbolic-full-name @{u}'
                     ,intern=T);
  commits <- if(length(tracking)==0) character(0) else {
    system(paste('git log',paste0(tracking,'..',branch),'--oneline')
           ,intern=T)};
  diffs <- lapply(diff_filters,git_diff_filter);
  if(print){
    message('Branch: ',branch);
    if(length(commits)>0) {
      message('Ahead of ',tracking,' by ',length(commits),' commit'
              ,if(length(commits)>1) 's.' else '.')} else {
                if(!any(sapply(diffs,length)>0)){
                  message('All local changes have already been pushed')}};
    # TODO: check for un-pulled upstream changes
    for(ii in names(diffs)) if(length(diffs[[ii]])>0){
      message(ii,':'); cat(paste(' ',diffs[[ii]]),sep='\n');}
    }
  invisible(list(branch=branch,tracking=tracking,commits=commits
                 ,diffs=diffs));
  }
gst <- git_status;

git_lsfiles <- function(...) {systemwrapper('git ls-files',...)};

git_other <- function(...){systemwrapper('git',...)};
git_ <- git_other;

git_add <- function(files,...){
  systemwrapper('git add',files=files,...)};
gadd <- git_add;

git_rename <- function(from,to,...){systemwrapper('git rename',from,to,...)};

git_move <- function(from,to,...) {systemwrapper('git mv',from,to,...)};

git_push <- function(...) {systemwrapper('git push',...)};
gp <- git_push;

git_newbranch <- function(branch,pushorigin=F,...){
  systemwrapper('git checkout -b',branch,...);
  if(pushorigin) systemwrapper('git push origin',branch);
}
gbr <- git_newbranch;

# TODO: detect conflicts in advance and ask what to do
git_merge <- function(which,fastfwd=getOption('git.fastfwd',F)
                      ,verbose=getOption('git.verbose',T),...){
  cmd <- paste('git merge',if(!fastfwd) '--no-ff' else '',...);
  if(verbose) message('Executing the following command:\n',cmd);
  system(cmd);}
gmr <- git_merge;

git_autoconf <- function(upstream=getOption('git.upstream'),...){
  # should only be run in an interactive context
  if(!'upstream' %in% system('git remote',intern=T) && !is.null(upstream)){
    systemwrapper('git remote add upstream',upstream);
  }
  # Set username and email
  if(length(.username <- system('git config user.name',intern=T))==0){
    message("Please type in your name as you want it to appear in git logs:");
    .username <- paste0('"',readline(),'"');
    systemwrapper('git config --global user.name',.username)};
  if(length(.useremail <- system('git config user.email',intern=T))==0){
    message("Please type in your email as you want it to appear in git logs:");
    .useremail <- paste0('"',readline(),'"');
    systemwrapper('git config --global user.email',.useremail)};
}

# By default incorporates upstream changes if they don't conflict with local 
# changes but overwrites 
# set mergestrategy to 'ours' to resolve conflicts in favor of local changes
# Or set it to '' to do whatever the default action is.
# The ... args get passed to the merge command
git_getupstream <- function(mergestrategy='theirs'
                            ,message='Merge with upstream'
                            ,fastfwd=getOption('git.fastfwd',F)
                            ,upstrmbranch=getOption('git.upstrmbranch','master')
                            ,localbranch=getOption('git.workingbranch','master')
                            ,...){
  git_autoconf();
  systemwrapper('git fetch upstream');
  git_checkout(which = localbranch);
  upstreamaddress <- paste0('upstream/',upstrmbranch);
  systemwrapper('git merge',upstreamaddress
                ,if(!fastfwd) '--no-ff' else ''
                ,if(mergestrategy!='') paste0('-X',mergestrategy) else ''
                ,'-m',paste0('"',message,'"'),...);
  result <- system('git diff --name-only --diff-filter=U',intern=T);
  if(length(result)>0){
    warning('Uh oh. There seem to be merge conflicts in the following files:\n'
            ,paste(result,collapse=', '),'\nAborting merge.\n');
    systemwrapper('git merge --abort')
    stop('The merge you attempted to do will need to be sorted out manually outside of R. If you are doing this as part of a class, please ask your instructor for help.')};
}
gup <- gitup <- git_getupstream;


#' Title: Add a pattern to a .gitignore file
#'
#' @param patterns A character vector of patterns to ignore. Required.
#'                 Always appended. If you need to un-ignore something
#'                 you will have to edit .gitignore manually.
#' @param ignorepath Path to .gitignore (you can have multiple ones)
#'                   current directory by default.
#' @param preamble What to put in the line/s before a set of ignore 
#'                 patterns. Empty line by default, set to NULL if you
#'                 want to not skip a line.
#'
#' @return NULL
#' @export
#'
#' @examples git_ignore(c('*.csv','*.tsv'))
git_ignore <- function(patterns,ignorepath='.',preamble='') {
  write(c(preamble,patterns),file.path(ignorepath,'.gitignore'),append=T)};

# TODO: git nagger

# renaming and remapping  ----
#' A function to re-order and/or rename the levels of a factor or 
#' vector with optional cleanup.
#'
#' @param xx            a vector... if not a factor will be converted to 
#'                      one
#' @param lookuptable   matrix-like objects where the first column will
#'                      be what to rename FROM and the second, what to
#'                      rename TO. If there is only one column or if it's
#'                      not matrix-like, the levels will be set to these
#'                      values in the order they occur in `lookuptable`.
#'                      If there are duplicated values in the first column
#'                      only the first one gets used, with a warning.
#'                      Duplicate values in the second column are allowed 
#'                      and are a feature.
#' @param reorder       Whether or not to change the order of the factor
#'                      levels to match those in `lookuptable`. True by
#'                      default (logical). By default is `TRUE`, is set to
#'                      `FALSE` will try to preserve the original order of
#'                      the levels.
#' @param unmatched     Scalar value. If equal to -1 and `reorder` is `TRUE` 
#'                      then unmatched original levels are prepended to the 
#'                      matched ones in their original order of occurence. If 
#'                      equal to 1, then appended in their original order of 
#'                      occurrence. If some other value, then they are all 
#'                      binned in one level of that name. The (empty) new ones 
#'                      always go to the end.
#' @param droplevels    Drop unused levels from the output factor (logical)
#' @param case          Option to normalize case (`'asis'`` leaves it as it was)
#'                      before attempting to match to `lookuptable`. One value 
#'                      only
#' @param mapnato       If the original levels contain `NA`s, what they should 
#'                      be instead. They stay `NA` by default.
#' @param remove        Vector of strings to remove from all level names (can
#'                      be regexps) before trying to match to `lookuptable`.
#' @param otherfun      A user-specified function to make further changes to
#'                      the original level names before trying to match and
#'                      replace them. Should expect to get the output from
#'                      `levels(xx)` as its only argument.
#' @param spec_mapper   A constrained lookup table specification that includes
#'                      a column named `varname` in addition to the two columns
#'                      that will become `from` and `to`. This is for extra
#'                      convenience in projects that use such a table. If you
#'                      aren't working on a project that already follows this
#'                      convention, you should ignore this parameter.
#' @param var           What value should be in the `varname` column of 
#'                      `spec_mapper`
#' @param fromto        Which columns in the `spec_mapper` should become the 
#'                      `from` and `to` columns, respectively. Vector of length
#'                      2, is `c('code','label')` by default.
factorclean <- function(xx,lookuptable,reorder=T,unmatched=1
                        ,droplevels=F,case=c('asis','lower','upper')
                        ,mapnato=NA,remove='"',otherfun=identity
                        ,spec_mapper,var,fromto=c('code','label')){
  if(!is.factor(xx)) xx <- factor(xx);
  lvls <- levels(xx);
  lvls <- switch (match.arg(case)
                   ,asis=identity,lower=tolower,upper=toupper)(lvls) %>% 
    submulti(cbind(remove,'')) %>% otherfun;
  levels(xx) <- lvls;
  # Check to see if spec_mapper available.
  if(!missing(spec_mapper)&&!missing(var)){
    lookuptable <- subset(spec_mapper,varname==var)[,fromto];
  }
  # The assumption is that if you're passing just a vector or something like
  # it, then you want to leave the level names as-is and just want to change
  # the ordering
  if(is.null(ncol(lookuptable))) lookuptable <- cbind(lookuptable,lookuptable);
  # can never be too sure what kind of things with rows and columns are getting 
  # passed, so coerce this to a plain old vanilla data.frame
  lookuptable <- data.frame(lookuptable[,1:2]) %>% setNames(c('from','to'));
  if(length(unique(lookuptable[,1]))!=nrow(lookuptable)) {
    lookuptable <- lookuptable[match(unique(lookuptable$from),lookuptable$from),];
    warning("You have duplicate values in the first (or only) column of your 'lookuptable' argument. Only the first instances of each will be kept");
  }
  if(reorder){
    extras <- data.frame(from=lvls,to=NA,stringsAsFactors = F) %>%
      subset(!from %in% lookuptable$from);
    lookupfinal <- if(unmatched==-1) rbind(extras,lookuptable) else {
      rbind(lookuptable,extras)};
  } else {
    lookupfinal <- left_join(data.frame(from=lvls,stringsAsFactors = F)
                             ,lookuptable,by='from') %>% 
      rbind(subset(lookuptable,!from%in%lvls));
  }
  # if the 'unmatched' parameter has the special value of -1 or 1, leave the 
  # original names for the unmatched levels. Otherwise assign them to the bin
  # this parameter specifies
  lookupfinal$to <- with(lookupfinal,if(unmatched %in% c(-1,1)){
    ifelse(is.na(to),from,to)} else {ifelse(is.na(to),unmatched,to)});
  # Warning: not yet tested on xx's that are already factors and have an NA
  # level
  lookupfinal$to <- with(lookupfinal,ifelse(is.na(from),mapnato,to));
  out <- factor(xx,levels=lookupfinal$from);
  levels(out) <- lookupfinal$to;
  if(droplevels) droplevels(out) else out;
}


#' into the specified existing or new level. That level is listed last
#' in the resulting factor.
#' @param xx A \code{vector} (required).
#' @param topn \code{numeric} for the top how many levels to keep (optional, default =4)
#' @param binto \code{character} which new or existing level to dump the other values in
cl_bintail <- function(xx,topn=4,binto='other'){
  if(!is.factor(xx)) xx <- factor(xx);
  counts <- sort(table(xx),decreasing = T);
  if(is.numeric(binto)) binto <- names(counts)[binto];
  keep <- setdiff(names(counts)[1:min(length(counts),topn)],binto);
  droplevels(
    factor(
      ifelse(
        xx %in% keep, as.character(xx), binto
        ),levels=c(keep,binto)));
}


#' Take a character vector and perform multiple search-replace 
#' operations on it.
#' @param xx A \code{vector} of type \code{character} (required)
#' @param searchrep A \code{matrix} with two columns of type \code{character} (required). The left column is the pattern and the right, the replacement.
#' @param method One of 'partial','full', or 'exact'. Controls whether to replace only the matching regexp, replace the entire value that contains a matching regexp, or replace the entire value if it's an exact match.
submulti <- function(xx,searchrep
                     ,method=c('partial','full','exact'
                               ,'starts','ends','startsends')){
  # if no method is specified by the user, this makes it take the first value
  # if a method is only partially written out, this completes it, and if the
  # method doesn't match any of the valid possibilities this gives an informativ
  # error message
  method<-match.arg(method);
  # if passed a single vector of length 2 make it a matrix
  if(is.null(dim(searchrep))&&length(searchrep)==2) searchrep<-rbind(searchrep);
  # rr is a sequence of integers spanning the rows of the searchrep matrix
  rr <- 1:nrow(searchrep);
  # oo will hold the result that this function will return
  oo <- xx;
  switch(method
         ,partial = {for(ii in rr)
           oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo)}
         ,full =    {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo)]<-searchrep[ii,2]}
         ,exact = {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,fixed=T)]<-searchrep[ii,2]}
           #oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo,fixed = T)}
         ,starts = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1]),searchrep[ii,2],oo)}
         ,ends = {for(ii in rr)
           oo <- gsub(paste0(searchrep[ii,1],'$'),searchrep[ii,2],oo)}
         ,startsends = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1],'$'),searchrep[ii,2],oo)}
  );
  oo;
}

#' Take a data.frame or character vector and a vector of grep targets and return
#' the values that match (for data.frame, column names that match). If no 
#' patterns given just returns the names
#' @param xx A \code{data.frame} or character vector (required)
#' @param patterns A character vector of regexp targets to be OR-ed
grepor <- function(xx,patterns='.') {
  if(is.list(xx)) xx <-names(xx);
  grep(paste0(patterns,collapse='|'),xx,val=T);
}

#' Usage: `xx<-mapnames(xx,lookup)` where lookup is a named character vector
#' the names of the elements in the character vector are what you are renaming
#' things TO and the values are what needs to be matched, i.e. what renaming things
#' FROM. If you set namesonly=T then it just returns the names, not the original
#' object.
mapnames<-function(xx,lookup,namesonly=F,...){
  xnames <- names(xx);
  idxmatch <- na.omit(match(xnames,lookup));
  newnames <- names(lookup)[idxmatch];
  if(namesonly) return(newnames);
  names(xx)[xnames %in% lookup] <- newnames;
  xx;
}

#' Example of using R methods dispatch
#' 
#' The actual usage is: `truthy(foo)` and `truthy()` itself figures
#' out which method to actually dispatch.
truthy <- function(xx,...) UseMethod('truthy');
truthy.logical <- function(xx,...) xx;
truthy.factor <- function(xx,...) truthy.default(as.character(xx),...);
truthy.numeric <- function(xx,...) xx>0;
truthy.default <- function(xx,truewords=c('TRUE','true','Yes','T','Y','yes','y')
                           ,...) xx %in% truewords;
truthy.data.frame <- function(xx,...) as.data.frame(lapply(xx,truthy,...));

# table utilities -----------------------------------
t_autoread <- function(file,...){
  # make sure prerequisite function exists
  if(!exists('tread')) {
    instrequire('devtools');
    .result <- try({
      devtools::install_github('bokov/trailR',ref='integration'); 
      library(trailR);});
    if(is(.result,'try-error')) return(getTryMsg(.result));
  }
  do.call(tread,c(list(file,readfun=autoread),list(...)));
}

#' Autoguessing function for reading most common data formats
autoread <- function(file,na=c('','.','(null)','NULL','NA')
                     # change this to identity to do nothing to names
                     ,fixnames=function(xx) {
                       setNames(xx,tolower(make.names(names(xx))))}
                     ,file_args=list(),...){
  args <- list(...);
  # allow file_args to be overridden by ... args, while preserving
  # order of ... 
  for(ii in intersect(names(args),names(file_args))) file_args[[ii]] <- NULL;
  args <- c(file_args,args);
  # check for text formats
  if(nrow(enc<-guess_encoding(file))>0){
    # try to read as a delimited file via fread
    txargs <- args[intersect(names(args),names(formals(fread)))];
    txargs$na.strings <- na;
    out <- try(as_tibble(do.call(fread,c(list(input=file),txargs)))
               ,silent = T);
    if(!is(out,'try-error')) return(fixnames(out));
    txargs <- args[intersect(names(args),names(formals(read_delim)))];
    txargs$na <- na;
    txargs$delim <- '\t';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!is(out,'try-error') && ncol(out)>1) return(fixnames(out)) else out_tab <- out;
    txargs$delim <- ',';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!is(out,'try-error')) return(fixnames(out));
    cat('\nGuessed encoding:\n');print(enc);
    stop(attr(out,'condition')$message);
  }
  # try various binary formats
  xlargs <- args[intersect(names(args),names(formals(read_xls)))];
  xlargs$na <- na;
  # xlsx
  sheets <- try(.Call('readxl_xlsx_sheets',PACKAGE='readxl',file),silent=F);
  if(!is(sheets,'try-error')){
    if(length(sheets)>1 && !'sheet' %in% names(xlargs)) warning(
      "\nMultiple sheets found:\n",paste(sheets,collapse=', ')
      ,"\nReading in the first sheet. If you want a different one"
      ,"\nplease specify a 'sheet' argument")
    return(fixnames(do.call(read_xlsx,c(list(path=file),xlargs))));}
  # xls
  sheets <- try(.Call('readxl_xls_sheets',PACKAGE='readxl',file),silent=F);
  if(!is(sheets,'try-error')){
    if(length(sheets)>1 && !'sheet' %in% names(xlargs)) warning(
      "Multiple sheets found: ",paste(sheets,collapse=', ')
      ,"\nReading in the first sheet. If you want a different one"
      ,"\nplease specify a 'sheet' argument")
    return(fixnames(do.call(read_xls,c(list(path=file),xlargs))));}
  # need to unzip the file?
  out <- try(unzip(file,list=T));
  if(!is(out,'try-error')){
    if(length(zfiles <- out$Name[out$Length>0])==1){
      out <- unzip(file,files=zfiles,exdir=tempdir());
      return(autoread(out,na=na,...));
    } else {
      print(zfiles);
      stop('Please unzip the input file and run autoread() 
           on the individual files inside it.');
    }
    }
  message('\nUnknown file type?\n');
  stop(attr(out,'condition')$message);
  }

#' Sumarize a table column
colinfo <- function(col,custom_stats=alist(),...){
  nn <- length(col);
  nona <- na.omit(col);
  isna <- is.na(col);
  coltab <- table(nona);
  out <- list(class=paste0(class(col),collapse=':')
              ,uniquevals=length(coltab)
              ,isnum=is.numeric(col)
              ,frc_int=if(is.numeric(nona)) mean(nona%%1==0) else 0
              ,n_nonmissing=nn-sum(isna)
              ,n_missing=sum(isna)
              ,frc_missing=mean(isna)
              ,n_nonrepeat=sum(coltab==1)
              ,frc_nonrepeat=sum(coltab==1)/length(nona)
              ,frc_max=paste0(sort(coltab,decreasing = T)[1:3],collapse=':')
  );
  for(ii in names(custom_stats)){
    out[[ii]] <- eval(custom_stats[[ii]],envir = out)};
  dots <- getParentDots();
  for(ii in names(dots)) out[[ii]] <- eval(dots[[ii]],envir=out);
  out;
  }

tblinfo <- function(dat,custom_stats=alist()
                    # some handy column groupers
                    ,info_cols=alist(
                       c_empty=frc_missing==1,c_uninformative=n_nonmissing<2
                      ,c_ordinal=uniquevals<10&isnum
                      ,c_tm=uniquevals==1&n_missing>0
                      ,c_tf=uniquevals==2,c_numeric=isnum&!c_ordinal
                      ,c_factor=uniquevals<20&!isnum
                      ,c_complex=!(c_ordinal|c_tm|c_tf|c_numeric|c_factor)
                    ),...){
  out <- bind_rows(sapply(dat,colinfo,custom_stats=custom_stats,simplify=F)
                   ,.id='column');
  for(ii in names(info_cols)) out[[ii]] <- eval(info_cols[[ii]],envir=out);
  dots <- getParentDots();
  for(ii in names(dots)) out[[ii]] <- eval(dots[[ii]],envir=out);
  class(out)<-c('dtdict',class(out));
  return(out);
}

#' Returns a vector of column names that contain data elements of a particular type
#' as specified by the user: "integer","POSIXct" "POSIXt", "numeric", "character", 
#' "factor" and "logical". 
vartype <- function(dat, ctype) {
  xx <- unlist(sapply(dat, class));
  idx <- which(xx %in% ctype);
  res <- names(xx)[idx];
  return(res)
}

#' Not yet ready:
#' 
# rebuild_dct <- function(dat=dat0,rawdct=dctfile_raw,tpldct=dctfile_tpl,debuglev=0
#                         ,tread_fun=read_csv,na='',searchrep=c()){
#   out <- names(dat)[1:8] %>% 
#     tibble(colname=.,colname_long=.,rule='demographics') %>% 
#     rbind(tread(rawdct,tread_fun,na = na));
#   if(length(na.omit(out$colname))!=length(unique(na.omit(out$colname)))){
#     stop('Invalid data dictionary! Duplicate values in colname column');}
#   out$colname <- tolower(out$colname);
#   #dct0 <- subset(dct0,dct0$colname %in% names(dat0));
#   shared <- intersect(names(dat),out$colname);
#   out[out$colname %in% shared,'class'] <- lapply(dat0[,shared],class) %>% sapply(head,1);
#   out$colsuffix <- gsub('^v[0-9]{3}','',out$colname);
#   if(debug>0) .outbak <- out;
#   # end debug
#   out <- left_join(out,tpl<-tread(tpldct,tread_fun,na=na)
#                    ,by=c('colsuffix','colname_long'));
#   # debug
#   if(debug>0){
#     if(nrow(out)!=nrow(.outbak)) 
#       stop('Number of rows changed in dct0 after join');
#     if(!identical(out$colname,.outbak$colname)) 
#       stop('colname values changed in dct0 after join');
#   }
#   # find the dynamic named vars in tpl
#   outappend <- subset(tpl,!varname %in% out$varname);
#   # make sure same columns exist
#   outappend[,setdiff(names(out),names(outappend))] <- NA;
#   out <- rbind(out,outappend[,names(out)]);
#   # end debug
#   out$c_all <- TRUE;
#   # replace strings if needed
#   if(!missing(searchrep)) {
#     out$colname_long <- submulti(out$colname_long,searchrep);}
#   return(out);
# }



# string hacking ---------------------------------------------------------------
#' Fancy Span (or any other special formatting of strings)
#' 
fs <- function(str,text=str,url=paste0('#',gsub('[^_a-z0-9]','-',tolower(str)))
               ,tooltip=alist(str),class='fl'
               # %1 = text, %2 = url, %3 = class, %4 = tooltip
               # TODO: %5 = which position 'str' occupies in fs_reg if 
               #       applicable and if not found, append 'str'
               ,template='[%1$s]: %2$s "%4$s"\n'
               # Turns out that the below template will generate links, but they
               # only render properly for HTML output because pandoc doesn't 
               # interpret them. However, if we use the markdown implicit link
               # format (https://pandoc.org/MANUAL.html#reference-links) we 
               # don't have to wrap links in anything, but we _can_ use fs()
               # with the new template default above to generate a block of 
               # link info all at once in the end. No longer a point in using
               # the fs_reg feature for this case, the missing links will be
               # easy to spot in the output hopefully
               #,template="<a href='%2$s' class='%3$s' title='%4$s'>%1$s</a>"
               ,dct=dct0,col_tooltip='colname_long',col_class='',col_url=''
               ,col_text='',match_col=c('varname','colname'),fs_reg=NULL
               ,retfun=cat
               #,fs_reg='fs_reg'
               ,...){
  # if a data dictionary is specified use that instead of the default values 
  # for arguments where the user has not explicitly provided values (if there
  # is no data dictionary or if the data dictionary doesn't have those columns,
  # fall back on the default values)
  if(is.data.frame(dct) && 
     length(match_col<-intersect(match_col,names(dct)))>0){
    dctinfo <- dct[match(str,do.call(coalesce,dct[,match_col])),];
     #!all(is.na(dctinfo <- dct[which(dct[[match_col]]==str)[1],]))){
    if(missing(tooltip) #&& 
      #length(dct_tooltip<-na.omit(dctinfo[[col_tooltip]]))==1) {
      #tooltip <- dct_tooltip;}
    ){tooltip <- do.call(coalesce,c(dctinfo[,col_tooltip],tooltip,''))};
    if(missing(text) && 
       length(dct_text<-na.omit(c(dctinfo[[col_text]],NA)))==1) {
      text <- dct_text;}
    if(missing(url) && 
       length(dct_url<-na.omit(c(dctinfo[[col_url]],NA)))==1) {
      url <- dct_url;}
    if(missing(class) && 
       length(dct_class<-na.omit(c(dctinfo[[col_class]],NA)))==1) {
      class <- dct_class;}
  } else dctinfo <- data.frame(NA);
  out <- sprintf(rep(template,nrow(dctinfo)),text,url,class,tooltip,...);
  # register each unique str called by fs in a global option specified by 
  # fs_register
  if(!is.null(fs_reg)) {
    dbg<-try(do.call(options,setNames(list(union(getOption(fs_reg),str))
                                      ,fs_reg)));
    if(is(dbg,'try-error')) browser();
    }
  retfun(out);
}

# Project Utilities ----
personalizeTemplate <- function(file,title='TITLE',author='AUTHOR'
                                ,deps=c('dictionary.R'),packages=c()
                                ,date=Sys.Date(),template='TEMPLATE.R'
                                ,path_to_global
                                ,paths=c('.','..','MyProject')){
  if(length(deps)>0){
    .files <- sapply(deps,function(ii) !is.null(find_path(ii,paths)));
    if(!all(.files)) stop(
"Of the files you specified in the 'deps' argument the following are missing:\n"
      ,paste0(deps[!.files],collapse=', '))};
  # make sure the template exists
  whichtemplate <- find_path(template,paths);
  if(is.null(whichtemplate)) stop(sprintf('Cannot find file %s',template));
  # make sure global.R exists
  if(missing(path_to_global)) path_to_global<-grep('global.R'
                                                   ,list.files(recursive = T)
                                                   ,val=T);
  if(length(path_to_global)==0) stop("Cannot find file 'global.R'");
  out <- sprintf(readLines(whichtemplate)
                 ,title # The title that will appear in the header
                 ,author # Author, ditto
                 ,format(date,'%d/%m/%Y') # Date, ditto
                 # packages (optional)
                 ,paste('c(',paste0("'",packages,"'",collapse=','),')') 
                 ,file # ...so the file knows it's own name!
                 # dependencies on previously run files
                 ,paste('c(',paste0("'",deps,"'",collapse=','),')')
                 # location of global.R
                 ,path_to_global[1]
  );
  write(out,file);
}

find_path <- function(file,paths=c('.','..')){
  # get the basename of the file
  filebase <- basename(file);
  # generate a search-paths for this file, starting with the path component
  # of 'file'
  filedirs <- if(filebase!=file) dirname(file) else c();
  filedirs <- normalizePath(unique(c(filedirs,paths)));
  # return the first full path in which the file is found to exist
  for(ii in file.path(filedirs,filebase)) if(file.exists(ii)) return(ii);
  return(c());
}

load_deps <- function(deps,scriptdir=getwd(),cachedir=scriptdir
                      ,fallbackdir='MyProject',envir=parent.frame()){
  if(length(deps)==0||identical(deps,'')){message('No dependencies.');return();}
  # what objects got loaded by this function
  loadedobj=c();
  for(ii in deps){
    # if a cached .rdata file for this dependency cannot be found...
    if(is.null(iicached<-find_path(paste0(ii,'.rdata')
                                   ,c(cachedir,scriptdir,fallbackdir)))){
      # run that script and create one
      if(!is.null(iiscript<-find_path(ii,c(scriptdir,fallbackdir)))){
        # TODO: modify all files to write their cached results to a user 
        # specified path if one is provided
        message(sprintf('Trying to initialize cache using script %s'
                        ,iiscript));
        system(sprintf('R -e ".workdir<-\'%s\'; source(\'%s\',chdir=T)"'
                       ,cachedir,iiscript));
        # again try to find a valid path to it
        iicached <- find_path(paste0(ii,'.rdata')
                              ,c(cachedir,scriptdir,fallbackdir));
        } else{
          # if cannot find script, error
          stop(sprintf('The script %s was not found',ii));
        }};
    # if there is still no cached .rdata found, error
    if(is.null(iicached)){
      stop(sprintf('The cached file for %s could not be found',iiscript));
      # otherwise, the cached .rdata now exists one way or another, load it
    } else {
      loadedobj <- union(loadedobj,tload(iicached,envir=envir));
      message(sprintf('Loaded data for %s from %s',ii,iicached));
      };
  }
  return(loadedobj);
}
