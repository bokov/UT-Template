# This is the file where custom functions, if any are needed, should be defined.
library(methods);

# Needed pre-library install ----
# Credit:
# http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function(){ # nodeps
  if(Sys.getenv('R_CONFIG_ACTIVE')=='rstudio_cloud') os <- "rscloud" else {
    if (!is.null(sysinf <- Sys.info())){
      os <- sysinf['sysname'];
      if (sysinf['sysname'] == 'Darwin') os <- "osx";
    } else { ## mystery machine
      os <- .Platform$OS.type;
      if (grepl("^darwin", R.version$os)) os <- "osx"
      else if (grepl("linux-gnu", R.version$os)) os <- "linux";
    }
  }
  tolower(os);
};

# Generalizable functions ----
# (might be moved to tidbits/SPURS in the future)
#' Determine whether a file is "plain-text" or some sort of binary format
#'
#'
#' A wrapper for \code{cut} that selects cut-points for equal counts rather than
#' equal distances.
#'
#' @param xx             a numeric vector which is to be converted to a factor by cutting.
#' @param bins           a single numeric variable indicating how many bins
#' @param probs          Based on \code{bins} and passed to the \code{quantile}
#'                       function. If cannot get \code{bins} to work, may need
#'                       to specify this argument manually.
#' @param ...            passed to \code{quantile} and \code{cut}. Good place to
#'                       put a \code{labels} argument. Make sure that none of
#'                       these optional arguments are \code{na.rm},
#'                       \code{probs}, \code{names}, \code{x}, or \code{breaks}
#'                       otherwise they will conflict with hardcoded arguments
#'                       passed by this function.
#' @param .error.recover If the number of breaks produces redundance quantiles
#'                       (e.g. when a quarter of the values in \code{xx} are 0
#'                       then the 25th percentile and all the quantiles smaller
#'                       than it will be 0).
#'
#' @return  Factor or numeric vector.
qcut <- function(xx,bins=3,probs=seq(0,1,length.out=round(bins)+1),...
                 ,.error.recover=TRUE){
  # validate arguments
  checkmate::qassert(xx,'*>1');
  if(not(checkmate::qtest(xx,'n>1'))){
    warning("The 'xx' argument is not numeric. Not guaranteed to work.")};
  checkmate::qassert(bins,'n+[2,]');
  checkmate::qassert(probs, 'N>2[0,1]')
  if(checkmate::test_names(.dotsnames <- names(alist(...)))){
    checkmate::assert_names(.dotsnames,type='unique'
                            ,disjunct.from = c('na.rm','probs','names','x'
                                               ,'breaks'));
    };
  breaks <- stats::quantile(xx,probs=probs,names=FALSE,na.rm=TRUE,...);
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf;
  out<-try(cut(xx,breaks,...),silent=TRUE);
  if(methods::is(out,'try-error')){
    if(.error.recover && grepl("'breaks' are not unique",out)){
      retry <- match.call(); retry$bins <- bins - 1; retry$xx <- xx;
      warning("The 'bins' argument is too large. Trying a smaller value.");
      suppressWarnings(return(eval(retry)))} else {
      stop(out)}
    };
  out;
}


#' Take a \code{coxph} model and create a survival curve stratified into
#' \code{bins} levels of the model's linear predictor to see how good the model
#' is at startifying risk groups.
#'
#' @param cph       A \code{coxph} fitted model (with a formula)
#' @param bins      A a single numeric variable indicating how many bins to
#'                  break the linear predictor from the \code{coxph} model
#'                  into. Passed to \code{qcut}. Three by default.
#' @param addcols   A character vector saying which columns from the original
#'                  data should be included
#' @param ...       Passed to \code{qcut}
#' @param .keepdata Whether to attach the data frame extracted from the
#'                  \code{coxph} model to the output via a \code{data}
#'                  attribute
#'
#' @return  A \code{survfit} object
coxph2survfit <- function(cph,bins=3,addcols=c(),...,.keepdata=TRUE){
  if(!checkmate::check_class(cph,'coxph')){
    warning("The 'cph' argument is not numeric. Not guaranteed to work.")};
  checkmate::qassert(bins,'n+[2,]');
  checkmate::qassert(addcols,'S*');
  if(length(.dotsnames<-setdiff(names(match.call())
                                ,c(names(formals(sys.function())),'')))>0){
    checkmate::assert_names(.dotsnames,type='unique'
                            ,disjunct.from = c('na.rm','names','x','breaks'));
  };
  datargs <- list(formula=cph,data=eval(cph$call$data));
  for(ii in intersect(addcols,names(datargs$data))){
    datargs[[ii]] <- as.name(ii)};
  dat <- do.call(get_all_vars,datargs) %>% augment(cph,newdata=.) %>%
    mutate(group=qcut(.fitted,bins=bins,...));
  sft <- cph$call; sft[[1]] <- bquote(survfit);
  out <- eval(sft) %>% update(.~group,data=dat);
  if(.keepdata) attr(out,'data') <- dat;
  environment(out) <- environment();
  out;
}

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

#' One stop shop for making execution actually quiet.
#'
#' @param expr      Expression to evaluate
#' @param warnings  If \code{TRUE} suppress warnings
#' @param messages  If \code{TRUE} suppress messages
#' @param startup   If \code{TRUE} suppress package startup messages
#' @param capture   If \code{TRUE} capture any output and return it wrapped in
#'                  \code{invisible()}
#'
#' @return Either NULL or a character string.
#' @export
#'
#' @examples suppress(library(survival))
suppress <- function(expr,warnings=TRUE,messages=TRUE,startup=TRUE
                     ,capture=FALSE){
  origsink <- sink.number();
  on.exit(suppressWarnings(if(sink.number(type='output')>origsink){
    sink(type='output');
    sink()}));
  f1 <- if(warnings) suppressWarnings else identity;
  f2 <- if(messages) suppressMessages else identity;
  f3 <- if(startup) suppressPackageStartupMessages else identity;
  f4 <- if(capture) function(xx){
    .junko<-invisible(capture.output(.junkm<-capture.output(xx,type='message')
                           ,type='output'))} else identity;
  f4(f3(f2(f1(expr))));
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
#' @param choices       Passed to \code{menu}
#' @param batchmode     If given, this is the value that this function will
#'                      return without displaying a menu if \code{interactive()}
#'                      is \code{FALSE}
#' @param autoresponse  If given, this is the value that this function will
#'                      return without displaying a menu OR checking whether its
#'                      in an interactive environment. This is a hook for
#'                      automated testing and CI use-cases.
#' @param title         Passed to \code{menu}
#' @param usenames      If \code{TRUE} (default) and \code{choices} has a
#'                      \code{names} attribute then prepend names to choices
#' @param namepattern   If \code{usenames} is \code{TRUE} and there are names,
#'                      combine them with values using this \code{sprintf}
#'                      pattern
#' @param graphics      Passed to \code{menu}
#' @param extramessage  Message that prints before the title of the menu. Or a
#'                      function that will be executed before the menu is
#'                      invoked.
#' @param ignorezero    If \code{TRUE} then instead of exiting, the menu will
#'                      re-display if the user chooses \code{0}
#' @param ignorezeromsg Message to send if \code{ignorezero} is \code{TRUE} and
#'                      0 has been typed.
#'
#' @return An integer corresponding to the choice the user made.
#' @export
#'
#' @examples
#' smartmenu(month.name)
#' smartmenu(month.name,batchmode=4)
#' smartmenu(month.name,batchmode=4,autoresponse=10)
#'
smartmenu <- function(choices,batchmode=1,autoresponse,title=NULL,usenames=TRUE
                      ,namepattern='%s\t-\t%s',graphics=FALSE,extramessage=c()
                      ,ignorezero=TRUE,ignorezeromsg='This is a required value'
                      ,responselog='.responselog'){
  if(!missing(extramessage)){
    if(is.function(extramessage)) extramessage() else message(extramessage)};
  if(!missing(autoresponse) && !is.null(autoresponse)) return(autoresponse);
  if(interactive()){
    if(usenames & !is.null(names(choices))){
      choices <- sprintf(namepattern,names(choices),choices)};
    out <- menu(choices=choices,graphics=FALSE,title=title);
    if(ignorezero) while(out==0) {
      message(ignorezeromsg);
      out <- menu(choices=choices,graphics=FALSE,title=title)};
    smartlog(out,deparse(match.call()));
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
#' @param cancelvalue  What to return if \code{ignorecancel} is \code{FALSE}
#'                     and the user does in fact cancel. If set to \code{stop}
#'                     (unquoted) then returns an error.
#'
#' @return Character string
#' @export
#'
#' @examples
smartfilechoose <- function(batchmode='',autoresponse,ignorecancel=TRUE
                            ,cancelvalue=NULL){
  if(!missing(autoresponse) && !is.null(autoresponse)) return(autoresponse);
  if(interactive()){
    fileselected <- FALSE;
    while(!fileselected){
      if(methods::is(out<-try(file.choose(),silent = TRUE),'try-error')){
        if(ignorecancel){
          message('This is a required file. Please make a selection.');
        } else {
          if(identical(cancelvalue,stop)) stop(attr(out,'condition'));
          smartlog(cancelvalue,deparse(match.call()));
          return(cancelvalue)};
      } else {
        # in case somebody tries to quote their file name
        out <- normalizePath(gsub('^[\'"]|[\'"]$','',out),winslash='/');
        if(file.exists(out)&&!file.info(out)$isdir){
          smartlog(out,deparse(match.call()));return(out)};
        if(!file.exists(out)) message(sprintf(
          "File '%s' not found. Please try again.",out)) else {
            if(!file.info(out)$isdir) message(sprintf(
              "'%s' is a directory, not a file",out))
          }}
      }};
    # out <- try(file.choose(),silent = TRUE);
    # if(ignorecancel){
    #   while(methods::is(out,'try-error')){
    #
    #     out <- try(file.choose(),silent=TRUE);
    #     }
    #   } else if(methods::is(out,'try-error')){
    #     if(identical(cancelvalue,stop)) stop(attr(out,'condition'));
    #     return(cancelvalue);
    #   }
    # return(out);
    # };
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
smartsetnames <- function(xx,names=base::names(xx),namepre='dat',namepad=2
                          ,namesuf='',maxlen=6
                          ,illegalchars='[^A-Za-z[:digit:]_]'
                          ,namepattern=paste0(namepre,'%0',namepad,'d'
                                              ,namesuf)
                          ,nameprevious=paste0('^',namepre,'[0-9]{',namepad
                                               ,'}$')){
  if(is.null(names)) names <- rep('',length(xx));
  names <- gsub(illegalchars,'',names);
  # this second round is to remove leading number and _ which are only illegal
  # at the beginning of a variable name. Also trim to size in this step
  names <- substr(sub('^[^A-Za-z.]+','',names),1,maxlen);
  names[is.na(names)] <- '';
  replace <- names!=make.names(names,unique=TRUE) |
    grepl(nameprevious,names);
  while(any(replace)){
    names[replace] <- sprintf(namepattern,seq_len(sum(replace)));
    replace <- names!=make.names(names,unique=TRUE);
  }
  if(missing(xx)) return(names) else return(setNames(xx,names));
}

#' An automation-friendly wrapper for readline.
#'
#' @param prompt       Passed to \code{readline}
#' @param batchmode    If given, this is the value that this function will
#'                     return without displaying a menu if \code{interactive()}
#'                     is \code{FALSE}
#' @param autoresponse If given, this is the value that this function will
#'                     return without displaying a menu OR checking whether its
#'                     in an interactive environment. This is a hook for
#'                     automated testing and CI use-cases.
#'
#' @return The character string typed in at the keyboard or \code{autoresponse}
#'         or \code{batchmode}
#' @export
#'
#' @examples
#' smartreadline('Type something: ',batchmode='Nothing')
#' smartreadline('Type something: ',autoresponse='Hello world')
#'
smartreadline <- function(prompt,batchmode='',autoresponse){
  if(!missing(autoresponse) && !is.null(autoresponse)) return(autoresponse);
  if(interactive()){
    out <- readline(prompt);
    smartlog(out,deparse(match.call()));
    return(out)};
  return(batchmode);
}

#' This function starts or continues a logfile named by the \code{file}
#' argument, and the logfile is simple commented R code for a list... but the
#' list doesn't automatically get terminated, that need to be done manually by
#' appending a ')' to it
smartlog <- function(value,comment,file='.logfile'){
  if(!file.exists(file)) cat('list(',file=file) else {
    cat('\n,',file=file,append=TRUE)};
  if(is.numeric(value)) cat(value,file=file,append=TRUE) else {
    cat('"',value,'"',file=file,sep = '',append=TRUE)};
  if(!missing(comment)) cat(' #',comment,file=file,append=TRUE)};

#' Internal function used by \code{simdata} for creating random labels for
#' discrete variables
makevarmap <- function(dt){
  data.frame(origname=colnames(dt)
             ,varname=smartsetnames(names = tolower(colnames(dt)),namepre='var'
                                    ,maxlen = 20)
             ,dispname=smartsetnames(names=colnames(dt),namepre='Var',maxlen=40
                                     ,illegalchars="[^[:alnum:] _.,':]")
             ,comment=NA)};

#' Return the number of decimal places used by a vector
#'
#' @param xx   Numeric vector
#' @param dmin The lowest number of decimal places to consider (by default,
#'             rounded to the nearest 1e+10)
#' @param dmax The highest number of decimal places to consider (by default,
#'             rounded to the nearest 1e-20)
#' @param tol  What fraction of the numbers have to be as precise or less
#'             precise than the selected decimal threshold (set to \code{1})
#'             for all of them to be at or below the threshold.
#'
#' @return Single numeric value indicating how much we can round the \code{xx}
#'         without changing the value of (most of) it.
#' @export
#'
#' @examples
#' decimals(runif(5))
#' decimals(1:5*10)
decimals <- function(xx,dmin=-10,dmax=20,tol=.9){
  xx <- na.omit(xx);
  if(length(xx)==0) return(0);
  decs <- seq(dmin,dmax,by=5);
  for(ii in decs) if(mean(xx==round(xx,ii))>=tol) break;
  for(jj in seq(decs[max(match(ii,decs)-1,1)]
                ,decs[min(match(ii,decs)+1,length(decs))])){
    if(mean(xx==round(xx,jj))>=tol) break;}
  return(jj);
}

#' Converts XXX.XX-YY.YYY strings, possibly with alpha prefixes and suffixes,
#' to numeric ranges when possible
#' ... is passed to unprefsuf()
ranges <- function(xx,sep='-',pad=2,...){
  out <- unprefsuf(strsplit(xx,sep)[[1]],...);
  # if cannot parse, give up and return NA
  if(length(out)!=2 ||
     length(unique(attr(out,'prefs')))>1 ||
     length(unique(attr(out,'suffs')))>1 ||
     any(is.na(outnum<-as.numeric(out)))) return(xx);
  outdec <- decimals(outnum);
  strpatt <- if(outdec > 0) paste0('%0',pad+outdec+1,'.',outdec,'f') else {
    paste0('%0',pad,'.f')};
  outseq <- do.call(seq,as.list(c(setNames(sort(outnum),c('from','to'))
                                  ,by=10^(-outdec))));
  outseq <- sprintf(strpatt,outseq);
  paste0(attr(out,'prefs'),outseq,attr(out,'suffs'));
}

#' Strip out prefixes and suffixes, return infix with the prefixes and suffixes
#' as attributes
unprefsuf <- function(xx,pattern="^([^0-9.-]*)([0-9.-]+)(.*$)"){
  prefs <- gsub(pattern,'\\1',xx);
  suffs <- gsub(pattern,'\\3',xx);
  infxs <- gsub(pattern,'\\2',xx);
  attr(infxs,'prefs') <- prefs;
  attr(infxs,'suffs') <- suffs;
  infxs;
}

#' Internal function used by \code{simdata}.
randomstr <- function(nn,minlen=2,maxlen=8,len
                      ,chars=c(LETTERS,letters,0:9,0:9)){
  replicate(nn,paste0(sample(chars,sample(minlen:maxlen,1),rep=T)
                      ,collapse=''));
}

simdata <- function(xx,nn=length(xx)){
  oo<-UseMethod('simdata');
  if(!is.data.frame(oo)){
    oo[sample(seq_along(oo),mean(is.na(xx))*length(oo))]<- NA};
  oo};


simdata.data.frame <- function(xx,nn=nrow(xx)){
  oo <- lapply(xx,simdata,nn);
  data.frame(oo);
}

simdata.Date <- function(xx,nn=length(xx)){
  as.Date(simdata.numeric(as.numeric(xx),nn),origin='1970-01-01')};

simdata.POSIXct <- function(xx,nn=length(xx)){
  as.POSIXct(simdata.numeric(as.numeric(xx),nn),origin='1970-01-01')};

simdata.default <- function(xx,nn=length(xx)){rep(NA,nn)}

simdata.numeric <- function(xx,nn=length(xx)){
  dec <- decimals(xx);
  unname(round(quantile(xx,runif(nn),na.rm=TRUE),dec));
}

simdata.factor <- function(xx,nn=length(xx)){
  oo <- sample(xx,nn,rep=TRUE);
  levels(oo) <- randomstr(length(levels(oo)));
  oo;
}

simdata.character <- function(xx,nn=length(xx)){
  if(length(unique(xx))<40){
    return(as.character(simdata.factor(factor(xx),nn)))};
  randomstr(nn);
}

# Sampling data ----
subsample <- function(xx,prob=c(.5,.5)){
  setNames(split(xx,sample(seq_along(prob),size=length(xx)
                           ,replace = TRUE,prob = prob))
           ,sprintf('subsample%03d',seq_along(prob)))}

resample <- function(xx,nn=5){
  setNames(replicate(nn,sample(xx,size=length(xx),replace=TRUE),simplify=FALSE)
           ,sprintf('resample%03d',seq_len(nn)))}

# Project-specific functions ----
# These functions will probably not be moved to a package because they make
# strong assumptions about file locations, variables, file-names, etc. and
# therefore would probably not work as expected outside this project template
# framework.

#' Load or render script dependencies if not already cached.
#'
#' @param deps        Character vector of script names.
#' @param scriptdir   Where to look for each script.
#' @param cachedir    Where to save the \code{SCRIPTNAME.R.rdata} output from
#'                    each script and where to first look for cached results if
#'                    they exist.
#' @param fallbackdir Where to look for each script that is not found in
#'                    \code{scriptdir}.
#' @param envir       Environment in which to evaluate scripts (recommend
#'                    leaving unaltered).
#' @param loadfn      Function for loading \code{SCRIPTNAME.R.rdata} cached
#'                    results
#' @param rendfn      String with name of function to run on each of the files.
#'                    If it's not from the \code{base} library it should be
#'                    fully qualified (i.e. like the default value). The two
#'                    anticipated values for this argument are
#'                    \code{'rmarkdown::render'} for generating reports together
#'                    with cached output and \code{'source'} for generating just
#'                    the output. Other functions might also do useful things,
#'                    but no guarantees.
#' @param debug       Integer. The higher the value, the more information is
#'                    returned to the console or report. For now the only check
#'                    done on it is whether it is \code{>0} (default is
#'                    \code{0}).
#' @param msg         Function used for printing messages. Should take a first
#'                    argument and a list as the second argument.
#' @param msgopts     List of options to pass to \code{msg}
#' @param ...         Arguments to pass to the function specified in
#'                    \code{rendfn} (advanced, could cause errors if
#'                    certain arguments are used)
#'
#' @return Character vector of objects created and saved by the scripts that
#'         have been loaded into the working environment.
load_deps2 <- function(deps,scriptdir=getwd(),cachedir=scriptdir
                      ,fallbackdir='scripts',envir=parent.frame()
                      ,loadfn=if(exists('tload')) tload else load
                      ,render=getOption('load_deps.render',TRUE)
                      ,debug=0,msg=getOption('ripcord.messagefun',message)
                      ,msgopts=list()
                      ,...){
  if(length(deps)==0||identical(deps,'')){message('No dependencies.');return();}
  # what objects got loaded by this function
  loadedobj=c();
  for(ii in deps){
    # if a cached .rdata file for this dependency cannot be found...
    if(is.null(iicached<-find_path(paste0(ii,'.rdata')
                                   ,c(cachedir,scriptdir,fallbackdir))) ||
       # or if render is requested AND the html is not found...
       (render && is.null(find_path(gsub('\\.r$','.html',ii,ignore.case = TRUE)
                                    ,c(cachedir,scriptdir,fallbackdir))))){
      # run that script and create one
      if(!is.null(iiscript<-find_path(ii,c(scriptdir,fallbackdir)))){
        # TODO: modify all files to write their cached results to a user
        # specified path if one is provided
        msg(sprintf('Trying to initialize cache using script %s'
                        ,iiscript));
        # if rendering the scriports and not just running them
        rcmd <- if(get_os() == 'windows') 'Rscript' else 'R';
        cmd <- if(render){
          sprintf('%1$s --no-restore -e ".workdir<-\'%2$s\';options(load_deps.render=TRUE);rmarkdown::render(\'%3$s\',output_dir=\'%2$s\');"'
                  ,rcmd
                  ,normalizePath(cachedir,winslash='/')
                  ,normalizePath(iiscript,winslash='/'))} else {
          sprintf('%1$s --no-restore -e ".workdir<-\'%2$s\';options(load_deps.render=FALSE);source(\'%3$s\',chdir=TRUE)"'
                  ,rcmd
                  ,normalizePath(cachedir,winslash='/')
                  ,normalizePath(iiscript,winslash='/'))};
        if(debug>0) message('load_deps.render:',getOption('load_deps.render'));
        if(debug>0) message('About to run:\n',cmd,'\n');
        if(debug==0) {
          .junk <- suppress(system(cmd,intern = TRUE),capture=TRUE) } else {
          system(cmd,intern=TRUE)};
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
      loadedobj <- union(loadedobj,loadfn(normalizePath(iicached,winslash='/'),envir=envir));
      #message(sprintf('Loaded data for %s from %s',ii,iicached));
    };
  }
  return(loadedobj);
}

find_path <- function(file,paths=c('.','..')){
  # get the basename of the file
  filebase <- basename(file);
  # generate a search-paths for this file, starting with the path component
  # of 'file'
  filedirs <- if(filebase!=file) dirname(file) else c();
  filedirs <- unique(normalizePath(c(filedirs,paths),winslash='/'));
  # return the first full path in which the file is found to exist
  for(ii in file.path(filedirs,filebase)) if(file.exists(ii)) return(ii);
  return(c());
}

current_scriptname <- function(default='INTERACTIVE_SESSION.R'
                               ,.scriptname=parent.frame(3)$ofile){
  return(c(.scriptname,gsub('\\.spin\\.Rmd$','.R',knitr::current_input())
           ,default)[1])};

choose_outcomes <- function(dat,criteria='c_safetf',nmax=1,random=TRUE
                            ,exclude=c()){
  out<-setdiff(do.call(v,list(criteria,dat)),exclude);
  if(length(out)>0){
    nmax <- min(length(out),nmax);
    out <- if(random) sample(out,nmax) else out[1:nmax]};
  comment <- if(length(out)>1) 'These variables were ' else {
    'This variable was '};
  comment <- paste0(comment,' arbitrarily chosen for demonstration purposes.
                    Please manually assign the outcome/s you actually want to use.');
  with_cm(out,comment)};

choose_predictors <- function(dat,criteria=c_safe,nmax=3,random=TRUE
                              ,exclude=c()){
  out <- choose_outcomes(dat,criteria=criteria,nmax=nmax,random=random
                         ,exclude=exclude);
  comment(out) <- gsub('outcome','predictor',comment(out));
  out;
}


c()