# small utils ------------------------------------------------------------------

# Credit: 
# http://conjugateprior.org/2015/06/identifying-the-os-from-r/
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

