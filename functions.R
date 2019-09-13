# small utils ------------------------------------------------------------------

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' This function takes a list of package names, loads them if they are
#' available, otherwise attempts to install each one and then again
#' attempts to load it.
instrequire <- function(pkgs # nodeps
                        ,quietly=TRUE
                        # the dependencies argument is ignored and is only here
                        # so that it doesn't end up in the '...'
                        ,dependencies=TRUE
                        ,repos=getOption('repos','https://cran.rstudio.com/')
                        ,...){
  pkgs_installed <- sapply(pkgs,require,character.only=TRUE);
  if(length(pkgs_needed <- names(pkgs_installed[!pkgs_installed]))>0){
    utils::install.packages(pkgs_needed,repos=repos,dependencies = TRUE,...);
    pkgs_final <- sapply(pkgs_needed,require,character.only=TRUE
                         ,quietly=quietly);
    if(!all(pkgs_final)){
      stop(c('the following required packages could not be installed:\n'
             ,paste0(names(pkgs_final[!pkgs_final]),collapse = ', ')));
    }
  };
}

clean_slate <- function(command="",removepatt='^\\.RData$|*.R\\.rdata$' # deps:git_subupd
                        ,all=TRUE,cleanglobal=TRUE
                        ,updatemodules=!file.exists('.developer')
                        ,envir=parent.frame()){
  if(!interactive()) warning('This function is intended to run in an '
                            ,'interactive session to restart that\n  '
                            ,'session on a clean slate. If you are calling it '
                            ,'non-interactively  (from a\n  script or '
                            ,'function), don\'t expect any code that you put '
                            ,'after it to work!');
  # remove cached files
  file.remove(list.files(pattern=removepatt,all.files=TRUE,recursive=TRUE,full.names = TRUE));
  # Update the git submodules
  if(updatemodules) git_subupd();
  # clear out calling environment
  rm(list=ls(all.names=all,envir = envir),envir = envir);
  # also global environment if specified
  if(cleanglobal) rm(list=ls(all.names=all,envir=.GlobalEnv),envir = .GlobalEnv);
  # if rstudioapi available, use it to restart the session
  if(require(rstudioapi) && rstudioapi::isAvailable()){
    rstudioapi::restartSession(command)};
}

#' Append or replace attributes of any object in a pipeline-frindly way.
#'
#' @param xx        Object whose attributes to modify and then return the object
#' @param rep.attrs Named list of attributes to create or replace
#' @param app.attrs Named list of attributes to create or append to
#'
#' @return The object `xx` with modified attributes
#' @export
#'
#' @examples
#' # Change an object's attribute
#' with_attrs(iris,list(class='list'))
#'
#' # Create a new attribute for an object
#' foo <- with_attrs(LETTERS,list(comment='Hello world'))
#' comment(foo)
#' foo <- with_attrs(foo,rep='One more comment.')
#' comment(foo)
#'
with_attrs<-function(xx,rep.attrs=list(),app.attrs=list()){ # nodeps
  attrs <- attributes(xx); if(is.null(attrs)) attrs<-list();
  for(ii in names(rep.attrs)) attrs[[ii]] <- rep.attrs[[ii]];
  for(ii in names(app.attrs)) attrs[[ii]] <- c(attrs[[ii]],app.attrs[[ii]]);
  attributes(xx) <- attrs;
  xx;
}

#' takes input and returns it with a comment attribute
cm <- with_cm <- function(xx,comment=NULL,append=T # deps:with_attrs
                          ,transf=stringr::str_squish){
  if(!is.null(transf)) comment <- transf(comment);
  if(append) with_attrs(xx,app.attrs=list(comment=comment)) else {
    with_attrs(xx,rep.attrs=list(comment=comment));
  }
}


getCall.list <- getCall.data.frame <- getCall.gg <- function(x,...) {attr(x,'call')};

# why not update calls?
update.call <- function(object,...){
  dots <- list(...);
  for(ii in names(dots)) object[[ii]] <- dots[[ii]];
  object;
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


# extract the error message of the argument
getTryMsg <- function(xx,ifNotErr=xx){ # revdeps: t_autoread
  if(methods::is(xx,'try-error')) return(attr(xx,'condition')$message);
  return(ifNotErr);}

# to be used inside a function to get a list of unevaluated calls
# from all the ... args
getParentDots <- function(xx,call=sys.call(-1),fun=sys.function(-1)){ # revdeps: colinfo,tblinfo
  out <- list();
  for(ii in setdiff(names(call),c(names(formals(fun)),''))){
    out[[ii]] <- call[[ii]]};
  out;
}

# Credit: http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function(){ # nodeps
  sysinf <- Sys.info();
  if (!is.null(sysinf)){
    os <- sysinf['sysname'];
    if (os == 'Darwin') os <- "osx";
  } else { ## mystery machine
    os <- .Platform$OS.type;
    if (grepl("^darwin", R.version$os)) os <- "osx";
    if (grepl("linux-gnu", R.version$os)) os <- "linux";
  }
  tolower(os);
};


systemwrapper <- function(cmd='',...,VERBOSE=getOption('sysverbose',T)
                          ,CHECKFILES=c('files')){ # nodeps
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
#' git checkout
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

#' List the files in the repo having a particular status
#'
#' Quoting from git documentation (\code{git help diff}):
#' \emph{Select only files that are Added (A), Copied (C), Deleted (D),
#' Modified (M), Renamed (R), have their type (i.e. regular file, symlink,
#' submodule, ...) changed (T), are Unmerged (U), are Unknown (X), or have had
#' their pairing Broken (B). Any combination of the filter characters (including
#' none) can be used. When * (All-or-none) is added to the combination, all
#' paths are selected if there is any file that matches other criteria in the
#' comparison; if there is no file that matches other criteria, nothing is
#' selected.}
#'
#' @param xx String containing one or more of A,C,D,M,R,T,U,X,B, or *
git_diff_filter <- function(xx) {
  system(paste('git diff --name-only --diff-filter',xx),intern=T)};

#' Nicely formatted and concise status of current git repo.
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

#' List only the files currently being tracked by git
git_lsfiles <- function(...) {systemwrapper('git ls-files',...)};

#' Whatever other git functions that aren't explicitly implemented yet. Just put
#' any combination of git arguments as arguments to this function, leaving out
#' \code{git} itself.
git_other <- function(...){systemwrapper('git',...)};
git_ <- git_other;

#' Make the specified file start getting tracked by the current git repository.
git_add <- function(files,...){
  systemwrapper('git add',files=files,...)};
gadd <- git_add;

#' Rename a git file, so git knows you didn't delete it.
git_rename <- function(from,to,...){systemwrapper('git rename',from,to,...)};

#' Move a git file, so git knows you didn't delete it.
git_move <- function(from,to,...) {systemwrapper('git mv',from,to,...)};

#' Push committed changes to the origin (for example, but not necessarily,
#' github.com)
git_push <- function(...) {systemwrapper('git push',...)};
gp <- git_push;

#' Create a new branch \emph{and} check it out immediately. Optionally also
#' push.
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

#' Delete and re-download git submodules if any exist.
#'
#' @param stopfile The name of a file which, if exists, will cause this function
#'                 to exit without doing anything. Will silently return errors
#'                 from shell but will not throw an error.
#'
#' @return If successful, \code{0}, otherwise an error code.
#' @export
#'
#' @examples
#'
#' \dontrun{ git_subupd() }
git_subupd <- function(stopfile='.developer'){if(!file.exists(stopfile)){
  unlink(systemwrapper("git submodule --quiet foreach 'echo $path'"
                       ,intern=TRUE,VERBOSE=FALSE)
         ,recursive = TRUE,force = TRUE);
  systemwrapper('git submodule update --init --recursive --remote')} else {
    message('Developer mode-- ignoring.'); return(0);
  }};

#' Automatically configure your global .gitconfig with your name and email
#' (if not yet thus configured) so that git will allow you to commit changes
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

#' Switch between ssh authentication and ssl authentication for a git repo.
#'
#' A use-case for this is some environments that by default initialize projects
#' as ssl/https (e.g. RStudio Cloud) but some users may prefer ssh
#' authentication. This easily converts between the two settings without having
#' to remember the whole git command. Will silently return errors from shell but
#' will not throw an error.
#'
#' @param tossh If `TRUE`, will attempt to convert the remote.origin.url from
#'              https to ssh. Default: `TRUE`
#' @param sshstr A string to use as the prefix for ssh connection. Optional,
#'               defaults to the values used by github.com
#' @param sslstr A string to use as the prefix for the ssl connection. Optional,
#'               defaults to the values used by github.com.
#'
#' @return Invisibly returns `0` or an error code.
#'
#' @export
#' @examples
#' \dontrun{
#' # Convert from https://github.com/... to git@github.com:...
#' git_ssh()
#'
#' # Convert from git@github.com:... to https://github.com/...
#' git_ssh(FALSE)
#'
#' }
git_ssh <- function(tossh=TRUE,sshstr='git@github.com:'
                    ,sslstr='https://github.com/'){
  currentorigin <- systemwrapper('git config remote.origin.url',intern=TRUE);
  message('Current origin: ',currentorigin);
  matchrepl <- if(tossh) c(sslstr,sshstr) else c(sshstr,sslstr);
  matchrepl[1]<-paste0('^',matchrepl[1]);
  neworigin <- gsub(matchrepl[1],matchrepl[2],currentorigin);
  message('Setting origin to: ',neworigin);
  systemwrapper('git config remote.origin.url',neworigin);
  systemwrapper('git remote -v');
}


# TODO: git nagger

# renaming and remapping  ----


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
  grep(paste0(patterns,collapse='|'),xx,value=TRUE);
}



# table utilities -----------------------------------

#' Extends trailR package with integrated universal (almost) file reader
#' 
#' @param file Any of the common delimited file formats
#' 
#' @export
t_autoread <- function(file,...){ #deps: getTryMsg
  # make sure prerequisite function exists
  if(!exists('tread')) {
    .result <- try({
      devtools::install_github('bokov/trailR',ref='integration');
      library(trailR)});
    if(methods::is(.result,'try-error')) return(getTryMsg(.result));
  }
  do.call(tread,c(list(file,readfun=autoread),list(...)));
}


#' Autoguessing function for reading most common data formats
#' 
#' Supported so far are: xls, xlsx, csv and most other delimited text formats,
#' SPSS, Stata, and SAS. 
#'
#' @param file       The name of a file you want to read into R
#' @param na         Vector of strings that should get translated to `NA` upon 
#'                   import. Optional, defaults to a reasonable set of values.
#' @param fixnames   A function that normalizes column names after importing the
#'                   data. If you want to leave them untouched, set this equal
#'                   to `identity()`. Optional, defaults to making them lower
#'                   case, R-legal, and unique.
#' @param file_args  This is to easily pass project-level or script-level
#'                   defaults in the form of an `alist()` to whichever lucky 
#'                   function ends up winning the contest to read your file. 
#'                   Only names that match the formal arguments of your function
#'                   will be used, the rest will be silently ignored. This way,
#'                   you can pass some `read_xlsx` specific arguments without 
#'                   worrying that something else will intercept them and error
#'                   out.
#' @param ...        Additional named arguments passed to this function will 
#'                   be added to those in `file_args` overriding any that have 
#'                   matching names.
#' 
#' @return A `tibble`
#' @importFrom readxl read_xls read_xlsx excel_sheets
#' 
autoread <- function(file,na=c('','.','(null)','NULL','NA')
                     # change this to identity to do nothing to names
                     ,fixnames=function(xx) {
                       stats::setNames(xx,tolower(make.names(names(xx),unique = TRUE)))}
                     ,file_args=list(),...){
  if(!file.exists(file)) stop(sprintf('File "%s" not found.'),file);
  if(dir.exists(file)) stop(sprintf('"%s" is not a file, it\'s a directory.'),file);
  args <- list(...);
  # allow file_args to be overridden by ... args, while preserving
  # order of ...
  for(ii in intersect(names(args),names(file_args))) file_args[[ii]] <- NULL;
  xlformat <- readxl::format_from_signature(file);
  args <- c(file_args,args);
  reader <- if(!is.na(xlformat)) paste0('read_',xlformat) else 'auto';
  if(reader == 'auto' && nrow(enc<-readr::guess_encoding(file))>0){
    # if it's a zip file, this unzips it and replaces the original file arg
    # with the temporary unzipped version
    unzfile <- suppressWarnings(utils::unzip(file,exdir = tempfile("autoread")));
    if(length(unzfile)>1){ if(!'sheet' %in% names(args)){
      warning(
        "\nMultiple files found in ",file,":\n"
        ,paste(list.files(dirname(unzfile),recursive = T,all.files = T)
               ,collapse=',')
        ,"\nReading in the first file. If you want a different one"
        ,"\nplease specify a 'sheet' argument");
      unzfile <- unzfile[1]} else unzfile <- unzfile[args$sheet]};
    if(length(unzfile==1)){
      message('Reading unzipped file',basename(unzfile));
      file <- unzfile;}
    message('Trying to read as a text file with fread()')
    # try to read as a delimited file via fread
    txargs <- args[intersect(names(args)
                             ,names(formals(data.table::fread)))];
    txargs$na.strings <- na;
    out <- try(tibble::as_tibble(do.call(data.table::fread
                                         ,c(list(input=file),txargs)))
               ,silent = T);
    if(!methods::is(out,'try-error')) return(fixnames(out));
    message('fread() failed! Falling back on read_delim');
    txargs <- args[intersect(names(args),names(formals(readr::read_delim)))];
    txargs$na <- na;
    txargs$delim <- '\t';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!methods::is(out,'try-error') && ncol(out)>1) return(fixnames(out)) else out_tab <- out;
    txargs$delim <- ',';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!methods::is(out,'try-error')) return(fixnames(out));
    cat('\nGuessed encoding:\n');print(enc);
    stop(attr(out,'condition')$message);
  }
  # try various binary formats
  if(reader %in% c('read_xls','read_xlsx')){
    # check for Excel formats
    message('checking sheets in workbook');
    sheets <- readxl::excel_sheets(file);
    if(length(sheets)>1 && !'sheet' %in% names(args)){
      warning(
        "\nMultiple sheets found:\n",paste(sheets,collapse=', ')
        ,"\nReading in the first sheet. If you want a different one"
        ,"\nplease specify a 'sheet' argument")};
    xlargs <- args[intersect(names(args)
                             ,names(formals(eval(as.name(reader)))))];
    xlargs$na <- na;
    message('About to read Excel file');
    out <- do.call(reader,c(list(path=file),xlargs));
    message('Fixing column names on Excel file');
    out <- fixnames(out);
    return(out)};

  # SPSS, SAS, and Stata
  # one of these has some error message that bubbles through despite silent=T
  # so we sink before the for loop, unsink if one of the readers succeeds...
  sink(tempfile());
  for(ff in c(haven::read_sav,haven::read_por,haven::read_dta,haven::read_sas
              ,haven::read_xpt)){
      {
        if(!methods::is(try(out <- ff(file),silent=T),'try-error')){
          sink();
          return(fixnames(out))}}
  }
  # and unsink at the end if none of them succeed
  sink();

  message('\nUnknown file type?\n');
  stop(attr(out,'condition')$message);
  }

#' Sumarize a table column
colinfo <- function(col,custom_stats=alist(),...){
  nn <- length(col);
  nona <- stats::na.omit(col);
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
              ,top3=paste0('"',names(sort(coltab,decreasing = T)[1:3]),'"'
                              ,collapse='; ')
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
                      # The below is an experiment with automatically adding
                      # explanatory labels to columns. Problem is, it's
                      # invisible to pander() and in View() is creates wide
                      # columns that get truncated anyway
                      # ,c_ordinal=with_attrs(uniquevals<10&isnum
                      #                       ,list(label=strwrap('Is this a numeric column that is candidate for converting to discrete values?',prefix='\n')))
                      ,c_tm=uniquevals==1&n_missing>0
                      ,c_tf=uniquevals==2,c_numeric=isnum&!c_ordinal
                      ,c_factor=uniquevals<20&!isnum
                      ,c_complex=!(c_ordinal|c_tm|c_tf|c_numeric|c_factor)
                    ),...){
  out <- dplyr::bind_rows(sapply(dat,colinfo,custom_stats=custom_stats,simplify=F)
                   ,.id='column');
  for(ii in names(info_cols)) out[[ii]] <- eval(info_cols[[ii]],envir=out);
  dots <- getParentDots();
  for(ii in names(dots)) out[[ii]] <- eval(dots[[ii]],envir=out);
  class(out)<-c('dtdict',class(out));
  return(out);
}


# string hacking ---------------------------------------------------------------

# Project Utilities ----
personalizeTemplate <- function(file,title='TITLE',author='AUTHOR'
                                ,deps=c('dictionary.R'),packages=c()
                                ,date=Sys.Date(),template='TEMPLATE.R'
                                ,path_to_global
                                ,paths=c('.','..','scripts')
                                ,notebook=F){
  # TODO: interactively prompt for non-default title and author if interactive
  # TODO: ask if want to be prompted for the other arguments if interactive
  if(length(deps)>0){
    .files <- sapply(deps,function(ii) !is.null(find_path(ii,paths)));
    if(!all(.files)) stop(
"Of the files you specified in the 'deps' argument the following are missing:\n"
      ,paste0(deps[!.files],collapse=', '))};
  # make sure the template exists
  whichtemplate <- find_path(template,paths);
  if(is.null(whichtemplate)) stop(sprintf('Cannot find file %s',template));
  # make sure global.R exists
  if(missing(path_to_global)) path_to_global<-find_relpath('global.R'
                                                           ,c('.','..','../..')
                                                           ,recursive = T
                                                           ,normalize = F);
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
  if(notebook) knitr::spin(file,knit=F);
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

find_relpath <- function(file,paths=c('..','../..','.'),recursive=F
                         ,normalize=T){
  filebase <- basename(file);
  paths<-c(if(filebase!=file && file.exists(dirname(file))){
    dirname(file)} else c(),paths);
  for(ii in paths){
    .paths <- file.path(c(ii,list.dirs(ii,full.names = T,recursive=recursive))
                        ,filebase);
    if(any(.found<-file.exists(.paths))){
      return(if(normalize) normalizePath(.paths[.found]) else .paths[.found])};
  }
  # if returns empty vector means none found
  return(c());
}

load_deps <- function(deps,scriptdir=getwd(),cachedir=scriptdir
                      ,fallbackdir='scripts',envir=parent.frame()
                      ,loadfn=if(exists('tload')) tload else load ){
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
        .junk <- system(sprintf('R --no-restore -e ".workdir<-\'%s\'; source(\'%s\',chdir=T)"'
                                ,cachedir,iiscript),intern = T);
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
      loadedobj <- union(loadedobj,loadfn(iicached,envir=envir));
      message(sprintf('Loaded data for %s from %s',ii,iicached));
      };
  }
  return(loadedobj);
}


#' Search for all sample datasets in all currently installed packages.
#'
#' A goal of this function is to be able to quickly filter through currently
#' available datasets and find ones that meet your needs so you're not using
#' the same old `mtcars` and `iris` for everything.
#'
#' @return A `data.frame` with columns `Package`: name of the package that 
#'         provides that dataset, `LibPath`: path where that package is 
#'         currently installed, `Item`: the name of the dataset, `Title`: a
#'         brief description of the dataset, `Class`: the class of the object
#'         listed in `Item` (if multiple classes, they are delimited by 
#'         semicolons),`IsDataFrame`: whether or not the object listed in 
#'         `Item` inherits from `data.frame`,`NumberNonNumeric`: number of
#'         columns that are not `numeric` (`character`, `factor`, `POSIXct`, 
#'         etc.),`Rows`: number of rows in the `Item` if applicable,`Cols`: 
#'         number of columns in the dataset in the `Item` if applicable.
#' @export
#'
#' @examples allTheData()
allTheData <- function(verbose=T){
  # get all datasets provided by all loaded packages
  dt = as.data.frame(data(package = .packages(all.available = TRUE))$results
                     ,stringsAsFactors=F);
  # df = data.frame?, nnn = number not numeric, nr/nc = nrows, ncols
  dt[,c('Class','IsDataFrame','NumberNonNumeric','Rows','Cols')] <- NA;
  for(ii in unique(dt$Package)){
    for(jj in subset(dt,Package==ii)$Item) {
      path <- paste0(ii,'::',jj);
      rows <- dt$Item==jj & dt$Package == ii;
      oo <- try(eval(parse(text=path)),silent=T);
      if(verbose) message(path);
      if(!is(oo,'try-error')){
        dt[rows,'class'] <- paste0(class(oo),collapse=';'); 
        if(dt[rows,'df'] <- is(oo,'data.frame')){
          dt[rows,'nnn'] <- ncol(oo) - sum(sapply(oo,is.numeric))};
        dt[rows,c('nr','nc')]<-c(c(nrow(oo),NA)[1]
                                                       ,c(ncol(oo),NA)[1]);}
    };
  return(dt);
}

