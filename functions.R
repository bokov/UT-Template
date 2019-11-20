# This is the file where custom functions, if any are needed, should be defined.

#' Determine whether a file is "plain-text" or some sort of binary format
#'
#' @param filename Path to the file
#' @param maxsize  Maximum number of bytes to read
#' @param textbytes Which characters are used by normal (though not necessarily
#'                  just ASCII) text. To detect just ASCII, the following value
#'                  can be used: `as.raw(c(7:16,18,19,32:127))`
#' @param tf       If `TRUE` (default) simply return `TRUE` when `filename`
#'                 references a text-only file and `FALSE` otherwise. If set to
#'                 `FALSE` then returns the "non text" bytes found in the file.
#'
#' @return boolean or raw
#' @export
#' @examples
#' library(datasets)
#' export(iris,"iris.yml")
#' isfiletext("iris.yml")
#' ## TRUE
#'
#' export(iris,"iris.sav")
#' isfiletext("iris.sav")
#' ## FALSE
#' isfiletext("iris.sav", tf=FALSE)
#' ## These are the characters found in "iris.sav" that are not printable text
#' ## 02 00 05 03 06 04 01 14 15 11 17 16 1c 19 1b 1a 18 1e 1d 1f
isfiletext <- function(filename,maxsize=Inf,
                       textbytes=as.raw(c(0x7:0x10,0x12,0x13,0x20:0xFF)),
                       tf=TRUE){
  bytes <- readBin(ff<-file(filename,'rb'),raw(),n=min(file.info(filename)$size,
                                                       maxsize));
  close(ff);
  nontextbytes <- setdiff(bytes,textbytes);
  if(tf) return(length(nontextbytes)==0) else return(nontextbytes);
}

isfilezip <- function(filename,return_ziptype=FALSE,
                      docpaths=c(MSO='[Content_Types].xml',
                                 OO='META-INF/manifest.xml'),...){
  if(!file.exists(filename)) stop(filename,' does not exist');
  contents <- try(unzip(filename,list=TRUE),silent=TRUE);
  # if there's an error, it's not a zip file
  if(is(contents,'try-error')){
    if(!return_ziptype) return(FALSE) else return(c())};
  # otherwise, it is
  if(!return_ziptype) return(TRUE);
  # if we care about the ziptype, match docpaths to what's in file
  matchedpaths <- docpaths %in% unzip(filename,list=TRUE)[,1];
  # if any of the docpaths match then return them
  if(any(matchedpaths)) return(names(docpaths[matchedpaths]));
  # otherwise, assume it's a plain zip file
  return('ZIP');
}

isfileyaml <- function(file,...){
  headers <- grepl(':$',readLines(file))
  isTRUE(sum(!headers) %% sum(headers) == 0)
}

try_import <- function(file,which=1,
                       trytext=c("xml","html","r","json","pzfx"),
                       trybin=c("dbf","dta","rda","rds","sas7bdat","sav","xls",
                                "xpt","matlab","fst","feather"),
                       verbose=1,...){
  # first try importing based on file extension
  out <- try(import(file = file, which = which, ...),silent=TRUE)
  if(!is(out,'try-error')) return(out)
  # zip formats
  switch(c(isfilezip(file,return_ziptype = TRUE),'NOTZIP')[1],
         MSO = out<-.try_formats(file,which,formats='xlsx',verbose=verbose,...),
         OO = out<-.try_formats(file,which,formats='ods',verbose=verbose,...),
         ZIP = c(),
         NOTZIP = c()
  )
  if(!is(out,'try-error')) return(out)
  # try text file methods
  if(isfiletext(file)){
    out <- .try_formats(file,which,trytext,verbose=verbose,...)
    if(!is(out,'try-error')) return(out)
    # if none of the above work, see if it's YAML
    if(isfileyaml(file)){
      out <- try(import(file=file,which=which,format='yaml',...),silent=TRUE)
      if(verbose > 1) message('Trying format: yaml')
      if(!is(out,'try-error')) return(out)
    }
    # otherwise, try fread
    out <- try(import(file=file,which=which,format='dat',...),silent=TRUE)
    if(verbose > 1) message('Trying format: delimited text')
    if(!is(out,'try-error')) return(out)
  }
  # now try the binary formats
  out <- .try_formats(file,which,trybin,verbose=verbose,...)
  if(!is(out,'try-error')) return(out)
}

.try_formats <- function(file,which,formats,verbose=1,...){
  for(ii in formats){
    if(verbose > 0) message('Trying format: ',ii)
    out <- try(import(file = file, format = ii, which = which, ...)
             ,silent=TRUE)
    if(is(out,'try-error')){
      out <- try(import(rv$infile,format=ii,which=1),silent=TRUE)
    if(!is(out,'try-error')){
      if (verbose > 0) warning('Specified table does not exist in file, ',
                               'extracting first available table instead')
      break}
    } else break}
  return(out)
}

#' Checks to see if there is an option with the user-specified name, and
#' if there isn't, creates one with the user-specified default. Returns the 
#' final value of that option.
#'
#' @param name    The option that should exist.
#' @param default What to set it to if it doesn't exist.
#'
#' @return The value of the \code{name} option.
#' @export
#'
#' @examples
#' optinit()
optinit <- function(name='.stacks',default=new.env()){
  oo <- options(name);
  if(is.null(oo[[name]])){
    oo[[name]] <- default;
    do.call(options,oo);}
  return(options(name)[[1]]);
};

#' Create a new persistent 'stack' of values that can be 'popped'
#'
#' @param xx   A vector or list.
#' @param name What to name the new stack (character)
#'
#' @return Value of \code{name}
#' @export
#'
#' @examples
#' stack(letters)
#' 
stack <- function(xx,name){
  stacks<-optinit('.stacks',new.env());
  if(missing(name)) name <- make.names(as.character(substitute(xx)));
  stacks[[name]] <- xx;
  return(name);
};

#' Return the top value of a stack and remove it from the stack
#'
#' @param xx       Name of stack
#' @param fallback What to return if the stack is empty or doesn't exist
#'
#' @return Either the previous first item in the stack or \code{fallback}
#' @export
#'
#' @examples
#' stack(letters[1:3],'teststack')
#' # since stack is not empty, the 'fallback' argument is ignored
#' pop('teststack','EMPTY!')
#' pop('teststack')
#' pop('teststack')
#' pop('teststack')
#' # now the stack is empty, so the 'fallback' argument is used
#' pop('teststack','EMPTY!')
pop <- function(xx,fallback=NULL){
  stacks<-optinit('.stacks',new.env());
  if(length(stacks[[xx]])==0) return(fallback);
  out <- stacks[[xx]][[1]];
  stacks[[xx]] <- stacks[[xx]][-1];
  return(out);
}

#' A wrapper for R's built-in menu with additional features.
#'
#' @param choices      Passed to \code{menu}
#' @param batchmode    If given, this is the value that this function will 
#'                     return without displaying a menu if \code{interactive()}
#'                     is \code{FALSE}
#' @param autoresponse If given, this is the value that this function will 
#'                     return without displaying a menu OR checking whether its
#'                     in an interactive environment. This is a hook for 
#'                     automated testing and CI use-cases.
#' @param title        Passed to \code{menu}
#' @param graphics     Passed to \code{menu}
#' @param extramessage Message that prints before the title of the menu. Or a 
#'                     function that will be executed before the menu is 
#'                     invoked.
#' @param ignorezero   If \code{TRUE} then instead of exiting, the menu will 
#'                     re-display if the user chooses \code{0}
#'
#' @return An integer corresponding to the choice the user made.
#' @export
#'
#' @examples
#' smartmenu(month.name)
#' smartmenu(month.name,batchmode=4)
#' smartmenu(month.name,batchmode=4,autoresponse=10)
#' 
smartmenu <- function(choices,batchmode=1,autoresponse,title=NULL
                      ,graphics=FALSE,extramessage=c(),ignorezero=TRUE){
  if(!missing(extramessage)){
    if(is.function(extramessage)) extramessage() else message(extramessage)};
  if(!missing(autoresponse) && !is.null(autoresponse)) return(autoresponse);
  if(interactive()){
    out <- menu(choices=choices,graphics=FALSE,title=title);
    if(ignorezero) while(out==0) {
      out <- menu(choices=choices,graphics=FALSE,title=title)};
    return(out);
  } else return(batchmode);
}

#' A wrapper for \code{file.choose()} that's friendly to automated testing.
#'
#' @param batchmode    If given, this is the value that this function will 
#'                     return without displaying a menu if \code{interactive()}
#'                     is \code{FALSE}
#' @param autoresponse If given, this is the value that this function will 
#'                     return without displaying a menu OR checking whether its
#'                     in an interactive environment. This is a hook for 
#'                     automated testing and CI use-cases.
#' @param ignorecancel If the user cancels, continue asking for a file.
#'
#' @return Character string
#' @export
#'
#' @examples
smartfilechoose <- function(batchmode='',autoresponse,ignorecancel=TRUE){
  if(!missing(autoresponse) && !is.null(autoresponse)) return(autoresponse);
  if(interactive()){
    out <- try(file.choose(),silent = TRUE);
    if(ignorecancel){
      while(methods::is(out,'try-error')){
        message('This is a required file. Please make a selection.');
        out <- try(file.choose(),silent=TRUE);
        }
      } else if(methods::is(out,'try-error')) stop(attr(out,'condition'));
    return(out);
    };
  return(batchmode);
};

#' A more customizable way to generate guaranteed legal and unique names
#'
#' @param xx           An object that has names, only required argument
#' @param names        Character vector of proposed names for the object 
#' @param namepre      The prefix to assign to auto-generated names
#' @param namepad      How many zeros to use for padding numeric infixes of 
#'                     auto-generated names
#' @param namesuf      The suffix to assign to auto-generated names
#' @param maxlen       The maximum length to which input names will be truncated
#' @param illegalchars A regexp matching characters that will be removed from 
#'                     names
#' @param namepattern  The \code{sprintf} pattern for auto-generated names. If
#'                     used, \code{namepre}, \code{namepad}, and \code{namesuf}
#'                     will be ignored, but in that case you are responsible for
#'                     setting the correct value for \code{nameprevious}.
#' @param nameprevious Pattern for recognizing existing auto-generated names,
#'                     to prevent endless loops.
#'
#' @return If \code{xx} provided, that object with legal and unique names. 
#'         Otherwise, just the names.
#' @export
#'
#' @examples
#' 
#' junknames <- replicate(26,paste0(sample(c(letters,LETTERS
#'                                           ,0:9
#'                                           ,c('.',' ','\t','\n','|','_','=',':',';','/'))
#'                                           ,sample(0:10,1),rep=TRUE),collapse=''))
#' junknames
#' 
#' smartsetnames(names=junknames)
#' 
#' namedobj <- setNames(LETTERS,junknames)
#' 
#' smartsetnames(namedobj)
#' 
smartsetnames <- function(xx,names=base::names(xx),namepre='dat',namepad=3
                          ,namesuf='',maxlen=6
                          ,illegalchars='[^A-Za-x[:digit:]_]'
                          ,namepattern=paste0(namepre,'%0',namepad,'d'
                                              ,namesuf)
                          ,nameprevious=paste0('^',namepre,'[0-9]{',namepad
                                               ,'}$')){
  if(is.null(names)) names <- rep('',length(xx));
  names <- substr(gsub(illegalchars,'',names),1,maxlen);
  names[is.na(names)] <- '';
  replace <- names!=make.names(names,unique=TRUE) |
    grepl(nameprevious,names);
  while(any(replace)){
    names[replace] <- sprintf(namepattern,seq_len(sum(replace)));
    replace <- names!=make.names(names,unique=TRUE);
  }
  if(missing(xx)) return(names) else return(setNames(xx,names));
}

c()