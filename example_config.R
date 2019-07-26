#' ---
#' title: "Generic R Project Configuration File"
#' author: "Alex F. Bokov, Ph.D."
#' date: "10/18/2018"
#' ---
#' 
#' Please copy this file to `config.R`, edit that copy, and copy it over to
#' the working directory whenever you check out this project. This is just an
#' example of what computer-specific variables should be set. This does not 
#' actually get called by `run.R`. A file needs to be called `config.R` in order
#' to be used as a source of configuration information by our scripts
#' 
#' This no longer has to be a CSV file-- xls, xlsx, and most delimited formats
#' will be automatically recognized now.
inputdata <- 'WHERE_I_KEEP_MY_DATA/MY_DATA_FILE.csv';

#' If your column names are on the first row and are followed by a few blank
#' rows for some reason before the actual data begins, that's usually okay and 
#' you don't have to do anything special. But, if the column names are _not_ on
#' the first row, you will need to skip the rows preceding them by using the 
#' optional `n_skip` variable. For example, if there are two rows of comments 
#' or empty space and your colum names are on row 3, then you would need to 
#' uncomment `n_skip` and set it to 2.
# n_skip <- 0;

#' Do you know what character is used to delimit your files? Usually it's either
#' comma or tab, but it could be anything. If you're not sure, leave this 
#' commented out, and the scripts will try to guess it for you.
#' 
#' Note: `file_delim` is not yet used
#file_delim <- '\t';

#' Your data dictionary file (if/when you have one, uncomment this line)
#'
#' Note: `dctfile_raw` is not yet used
#dctfile_raw <- 'WHERE_I_KEEP_MY_DATA/MY_DATA_DICTIONARY.csv';

#+ echo=F,eval=F
c()
