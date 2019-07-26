# Set up user.name and user.email ----

if(!is.null(attr(system('git config --global user.name',intern=T),'status'))){
  .username <- readline('Please type in the user name you want to use and hit enter: ');
  system(sprintf('git config --global user.name "%s"',.username));
}

if(!is.null(attr(system('git config --global user.email',intern=T),'status'))){
  .useremail <- readline('Please type in the email address you want to use and hit enter: ');
  system(sprintf('git config --global user.email "%s"',.useremail));
}

# Set up useful aliases ----
if(!is.null(attr(system('git config alias.upd',intern=T),'status'))){
  system('git config --global alias.upd "submodule update --init --recursive --remote"')
  } else warning('You already have a git alias named "upd", leaving it unchanged.');

if(!is.null(attr(system('git config alias.lg',intern=T),'status'))){
  system('git config --global alias.lg "log --oneline --graph --color --all --decorate"')
  } else warning('You already have a git alias named "lg", leaving it unchanged.');

if(!dir.exists('scripts') && dir.exists('../scripts')) setwd('..')

if(dir.exists('scripts')){
  system('git upd');
  if(dir.exists('scripts/hooks')) file.copy()
}
#else warning('
#  This script should be run in a directory that has a "scripts" subdirectory.
#  Cannot find that, so not installing git hooks');



c()
