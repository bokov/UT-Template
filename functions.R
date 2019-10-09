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

try_import <- function(file,which,
                       trytext=c("xml","html","r","json","pzfx"),
                       trybin=c("dbf","dta","rda","rds","sas7bdat","sav","xls",
                                "xpt","matlab","fst","feather"),
                       verbose=1,...){
  # first try importing based on file extension
  out <- try(import(file = file, which = which, ...),silent=TRUE)
  if(!is(out,'try-error')) return(out)
  # zip formats
  switch(isfilezip(file,return_ziptype = TRUE),
         MSO = out<-import(file=file,which=which,format='xlsx',...),
         OO = out<-import(file=file,which=which,format='ods',...),
         ZIP = c()
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