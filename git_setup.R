# Set up user.name and user.email ----

suppressWarnings(if(!is.null(attr(system('git config --global user.name'
                                         ,intern=T)
                                  ,'status'))){
  .username <- readline('Please type in the user name you want to use and hit enter: ');
  system(sprintf('git config --global user.name "%s"',.username));
});

suppressWarnings(if(!is.null(attr(system('git config --global user.email'
                                         ,intern=T)
                                  ,'status'))){
  .useremail <- readline('Please type in the email address you want to use and hit enter: ');
  system(sprintf('git config --global user.email "%s"',.useremail));
});

# Set up useful aliases ----
if(!is.null(attr(suppressWarnings(system('git config alias.upd',intern=T))
                 ,'status'))){
  system('git config --global alias.upd "submodule update --init --recursive\
         --remote"')} else message(
           'You already have a git alias named "upd", leaving it unchanged.');

if(!is.null(attr(suppressWarnings(system('git config alias.lg',intern=T))
                 ,'status'))){
  system('git config --global alias.lg "log --oneline --graph --color --all\
         --decorate"')} else message(
           'You already have a git alias named "lg", leaving it unchanged.');

if(basename(getwd())=='scripts') setwd('..');
if(!file.exists('.gitmodules')){
  if(file.exists('../.gitmodules')) setwd('..') else {
    if(file.exists('../../.gitmodules')) setwd('../..') else {
      stop('Cannot find the .gitmodules file, cannot update submodules')
      }}
}

# delete the current submodule directories and recreate fresh
# if you are one of the developers for this project, create a file 
# named .developer in the directory that contains the .gitmodule file
# otherwise this script will blow away the submodule directories before
# updating them
if(!file.exists('.developer')){
  unlink(system("git submodule --quiet foreach 'echo $path'",intern=T)
       ,recursive = T,force = T)};
system('git upd');

#if(dir.exists('scripts/hooks')) file.copy();
#else warning('
#  This script should be run in a directory that has a "scripts" subdirectory.
#  Cannot find that, so not installing git hooks');

if(file.exists('scripts/bootstrap.Rprofile')) file.copy('scripts/bootstrap.Rprofile','.Rprofile');

c()
