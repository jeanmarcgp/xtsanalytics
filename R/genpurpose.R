####################################################################################
# FILE genpurpose.R
#
#
#  This contains the general purpose functions for the package.
#
#  BUGS:
#  -----
#  .load.universe - global environment won't save.
#
#  LIST OF FUNCTIONS:
#  ------------------
#
#  .startpoints         Instead of endpoints, for the beginning of a period + offset
#  .split_vector        Splits a vector into chunks of size k
#  .cumsum_na           Identical to cumsum but ignores NAs
#  .cumprod_na          Identical to cumprod but ignores NAs
#  .fnamestamp          Adds a timestamp to a file name
#  .getname             Returns the name of the object provided as the argument
#  .starttime           Logs current system time
#  .elapsedtime         Reports / prints time elapsed since start.time
#  .save_Rdata          Saves the specified object to an .Rdata file with timestamp
#  .sprint              Print on console using sprintf formatting
#  .trimspaces          Trims white spaces from a string
#  .recycle             Recycle an object to a target length
#  .recycle_better      Recycle a vector using names and smart rules
#  .emptyxts            Make an xts matrix with colnames
#
#
###################################################################################
#
#   General Purpose Functions
#
###################################################################################

#
#
#----------------------------------------------------------------------------------
# FUNCTION startpoints
#
#' Finds the beginning of a period in an xts matrix
#'
#' This function is similar to endpoints except that it finds the start of the
#' period rather than the end of period.
#'
#' @param x        an xts matrix
#' @param on       the string name for the period to extract (like endpoints)
#' @param offset   the offset from the start of the period (offset = 1 at start date)
#' @param k        how many periods to skip (k=1 means none are skipped)
#'
#' @return Returns a vector of the row positions in the matrix
#' @export
#----------------------------------------------------------------------------------

startpoints <- function (x, on = "months", offset = 1, k = 1) {
  head(endpoints(x, on, k) + offset, -1)
}



#----------------------------------------------------------------------------------
#' Split a vector into chunks of size k.
#'
#' This functions splits a vector into chunks of size k, and returns a list of
#' vectors.  The last vector in the list will be less than length k if k doesn't
#' divide the length of the vector exactly.
#'
#' Parameter k must be of type numeric.  If not an integer, it will be coerced
#' to an integer using trunc().
#'
#' Parameter vec must be a vector but can contain any element type.
#'
#' @param  vec   The vector to split into chunks.
#' @param  k     The size of each chunks.
#' @return A list of vectors, each vector of size k, except possibly the last one.
#' @examples
#' split_vector(c(1,2,3,4,5), 3)
#' @export
#----------------------------------------------------------------------------------
split_vector <- function(vec, k) {
  # Basic checks
  stopifnot(is.vector(vec), is.numeric(k))
  k <- trunc(k)

  # Create a list of each chunks
  lvec <- split(vec, ceiling(seq_along(vec)/k))
  return(lvec)

}   ######  END split_vector  ######


#----------------------------------------------------------------------------------
#' Compute cumulative sums or products and ignores NAs.
#'
#' Functions cumsum_na() and cumprod_na() are similar to cumsum() and cumprod(),
#' except that they ignore any NAs present within the argument matrix.
#'
#' NAs are ignored by assuming they equal zeroes for cumsum_na() and equal
#' ones for cumprod_na(). These functions work on vectors and xts matrices.
#' With an xts matrix, it applies cumprod or cumsum on every column.
#'
#' @param x Value of the series from which to compute the cumulative sum or product.
#' @return A numeric vector of the cumulative sum or product.
#' @examples
#' cumsum_na(c(NA, 1, 2, 1, NA, 0, 0))
#' cumprod_na(c(NA, 1, 2, 1.3, NA, 1, 2.5, NA))
#'
#' # compute returns on xts_data, first row contains NAs
#' rets <- ROC(xts_data, type="discrete")
#' ecurves <- cumprod_na(1 + rets)
#' xtsplot(ecurves, main = 'Equity curves')
#' xtsplot(xts_data, main = 'Prices')   # identical since normalized.
#'
#' @export
#----------------------------------------------------------------------------------
cumsum_na <- function(x) {

  x[is.na(x)] <- 0
  return(cumsum(x))
}

#' @describeIn cumsum_na Compute the cumulative sum while ignoring NAs
#' @export
cumprod_na <- function(x) {

  x[is.na(x)] <- 1
  return(cumprod(x))

}


#----------------------------------------------------------------------------------
#  FUNCTION fnamestamp
#
#' Extends a file name with a datetime stamp.
#'
#' Returns a character string (the filename) with a datetime stamp appended for
#' easy output file naming.
#'
#' @param fname   Name of the file, with or without the extension.  The datetime
#'                stamp will be inserted just before the extension, or at the end
#'                if the file has no extension specified.  An extension is any
#'                string after the last character dot in fname.
#' @param asis    Logical.  If TRUE, the time stamp is appended at the end, even
#'                if an extension exists.
#' @return A character string made up of the filename, the datetime stamp and the
#'         extension (located either before or after the datetime stamp).
#' @examples
#' fnamestamp('./my cool file name.pdf')
#' fnamestamp('./my other cool file name.pdf', asis=TRUE)
#' @export
#----------------------------------------------------------------------------------
fnamestamp <- function(fname="NoName", asis=FALSE) {
  #  Get current system time
  st <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  if(asis) {
    fn2 <- paste0(fname, " ", st)
  } else {
    #  Extract fname extension (after a dot .)
    #  strsplit returns a list of 1 char vector - simplify it.
    fname.split <- strsplit(fname, ".", fixed=TRUE)[[1]]
    flen <- length(fname.split)
    if(flen > 1) {
      #  Extract main part with path and name extension
      fname.ext <- paste0(".", fname.split[[flen]])
      fname.len <- nchar(fname)
      fname.main <- substr(fname, 1, fname.len-nchar(fname.ext))

      #  Create new name
      fn2 <- paste0(fname.main, " ", st, fname.ext)

    } else {
      fn2 <- paste0(fname, " ", st)
    }
  }
  return(fn2)

}   ######  END fnamestamp  ######


#----------------------------------------------------------------------------------
#  FUNCTION getname
#
#' Get the name of an object
#'
#' @param  object  The object from which its name is sought.
#' @return Returns the name of the object passed as argument, in the form of a string.
#'
#' @examples
#' myobject <- 5
#' getname(myobject)
#'
#' @export
#----------------------------------------------------------------------------------
getname <- function(object) {
  name <- deparse(substitute(object))
  return(name)

}  ######  END getname  ######


#----------------------------------------------------------------------------------
#  FUNCTIONS starttime, elapsedtime
#
#' Start a timer and report the elapsed time
#'
#' These two functions, `starttime()` and `elapsedtime()` work together.
#' Call starttime() at the beginning of a script
#' without any argument.  This logs the current time in global variable `.__start.time`.
#' Then at the end of the script or whenever a report of the elapsed time interval is
#' desired, call elapsed.time() to print the elapsed time on the console.
#'
#' NOTE:  The reference to global variable `.__start.time` should be changed to using
#' the package NAMESPACE, since this provides the side effect of creating a global
#' variable.  But for now, this works fine, so this is a future improvement.
#'
#' @param  script_end  Logical. Default value TRUE. Only relevant for elapsedtime().
#'                     If TRUE, an end of script message is printed on the console
#'                     in addition to the elapsed time.  If FALSE, only the elapsed
#'                     time is reported.
#' @return No value returned for starttime().  The elapsed time is returned using elapsedtime().
#'
#' @examples
#' starttime()
#' Sys.sleep(0.5)
#' zzz <- elapsedtime()
#'
#' @export
#----------------------------------------------------------------------------------
starttime <- function() {
  assign(".__start.time", Sys.time(), envir = .GlobalEnv)
}

#' @describeIn starttime Report the elapsed time since starttime() was last invoked.
#' @export
elapsedtime <- function(script_end = TRUE) {
  elapsed.time <- Sys.time() - .__start.time
  sprint("")
  print(elapsed.time)
  if(script_end) sprint("Script completed successfully.")
  return(elapsed.time)
}



#----------------------------------------------------------------------------------
#  FUNCTION save_Rdata
#
#'  Save an object to an .Rdata file with dateTime stamp and file path.
#'
#' @param  path   File path, which usually starts with ./ and ends with /
#' @param  fname  File name
#' @param  object An object in quoted strings, or a list of objects,
#'                found in the parent scope.  Can be an environment
#'                or a variable.
#'
#' @return Side effect of saving the object to the specified file.
#'
#' @export
#----------------------------------------------------------------------------------
save_Rdata <- function(object=NULL, fname='temp', path='./') {
  fullpath <- paste0(path, fnamestamp(fname), '.Rdata')
  sprint("\nSaving object [ %s ] as:\n  %s ", object, fullpath)
  #  Save takes a list of strings as names of objects.
  #  object is a string of parent scope variables.
  save(list=object, file=fullpath)
  sprint("%s saved.", object)

}   ######  END save_Rdata  ######


#----------------------------------------------------------------------------------
#  FUNCTIONS sprint and sprintcat
#'
#' Print to console using C-like sprintf formatting.
#'
#' A simple function to print text and variable content on the console, using
#' the C-like function sprintf formatting style.
#'
#' There are two versions of this function:  sprint and sprintcat.
#' Function sprint automatically appends a carriage return at the end of
#' the string whereas sprintcat does not.
#'
#' @param  string  A character string containing text and formatting style.
#'
#' @param  ...     Additional parameters passed to sprintf.  In most cases, these
#'                 are the sequence of variables that are printed, provided they
#'                 have an associated formatting in the string.
#'
#' @return No return value.  This function is called for its side effects
#'         by printing on the console.
#'
#' @seealso sprintf()
#'
#' @export
#----------------------------------------------------------------------------------
sprint <- function(string, ...) {
  str2 <- paste0(string, "\n")

  str_out <- sprintf(str2, ...)
  cat(str_out)

}   ######  END FUNCTION sprint ######

#' @describeIn sprint Similar to sprint but without the carriage return
#' @export
sprintcat <- function(string, ...) {

  str_out <- sprintf(string, ...)
  cat(str_out)

}   ######  END FUNCTION sprintcat ######


#----------------------------------------------------------------------------------
#  FUNCTION trimspaces
#
#'  Remove leading or trailing white spaces from a character string
#'
#'
#' @param x     The character string to process
#' @param type  Specifies whether to remove the leading, trailing or
#'               both white spaces.
#'
#' @return Return the string provided without leading or trailing white
#'         spaces.
#'
#' @export
#----------------------------------------------------------------------------------
trimspaces <- function(x, type=c("both", "leading", "trailing")) {
  switch(type[1],
         both = {
           retval <- gsub("^\\s+|\\s+$", "", x)
         },
         leading = {
           retval <- sub("^\\s+", "", x)
         },
         trailing = {
           retval <- sub("\\s+$", "", x)
         },
         #  Default expression
         stop('Function trim.spaces:  type is unknown.'))

  return(retval)

}  ######  END trimspaces  ######

#----------------------------------------------------------------------------------
#  FUNCTION recycle
#
#' Recycle a data object and truncate to a length of N.
#'
#' @param  data  Object containing the data to recycle
#' @param  N     The target length of the object once recycled.
#'
#' @return Returns the same object as provided, recycled enough times
#'         and truncated to ensure it has length N.
#'
#' @export
#----------------------------------------------------------------------------------
recycle <- function(data, N) {
  dlen    <- length(data)
  Nrep    <- ceiling(N / dlen)
  outdata <- rep(data, Nrep)[1:N]
  return(outdata)
}


#----------------------------------------------------------------------------------
#  FUNCTION recycle_better
#
#' Recycle a vector using names and smart rules
#'
#' @param vec       A numeric vector used for recycling.  If the vector is
#'                  not named, then it is simply recycled N times where
#'                  N = length(vecnames). If it is a named numeric, then
#'                  a vector of length N is built using the default argument
#'                  below, then those named values are inserted in the
#'                  result.
#'
#' @param vecnames  The ordered names to assign to the results.
#'
#' @param default   The default value to pad the vector with when a named
#'                  vec argument is provided.
#'
#' @return Returns a vector of length(vecnames) that is named and ordered
#'         as vecnames.
#'
#' @export
#----------------------------------------------------------------------------------
recycle_better <- function(vec, vecnames, default = 0) {

  # ######  for testing  ##########
  # vecnames = c("SPY", "IEV", "SHY", "GLD")
  # vec = 1
  #
  # ###############

  if(class(vecnames) != "character") stop("vecnames must be a string!")
  if(class(vec) != "numeric")        stop("vec must be a numeric vector!")

  #--------------------------------------------------------------
  # Recycle vec if a plain numeric (not named)
  #--------------------------------------------------------------
  if(is.null(names(vec))) {
    # Recycle numeric vec and assign names
    vec        <- recycle(vec, length(vecnames))
    names(vec) <- vecnames

  } else {
    #------------------------------------------------------------
    # vec is named, so pad the rest with default values
    #------------------------------------------------------------
    # First, get rid of names not in vecnames, if any
    vec                 <- vec[names(vec) %in% vecnames]
    tempvec             <- recycle(default, length(vecnames))
    names(tempvec)      <- vecnames
    tempvec[names(vec)] <- vec
    vec                 <- tempvec
  }

  return(vec)

}

#----------------------------------------------------------------------------------
#  FUNCTION emptyxts
#
#' Create an empty xts matrix filled with NAs
#'
#'
#' @param  cnames     A character vector of column names, or NA
#'                    if not specified.
#'
#' @param  nc         The number of columns unless cnames is provided,
#'                    which overrides this parameter.
#'
#' @param rowfill     A vector of numbers which is used to fill each
#'                    row of the xts matrix.  If the vector is too short,
#'                    it is recycled, or truncated if too long.  If NA, then
#'                    the xts will be empty (all NAs).
#'
#' @param  order.by   The index of the xts matrix
#'
#' @return An empty xts matrix filled with NAs
#'
#' @export
#----------------------------------------------------------------------------------
emptyxts <- function(cnames = NULL, nc = 1, rowfill = NA,
                     order.by = index(xts_data["2014", ])) {

  nr   <- length(order.by)
  if(is.null(cnames)) {
    mat <- matrix(data = NA, nrow = nr, ncol = nc)
  } else {
    nc  <- length(cnames)
    mat <- matrix(data = NA, nrow = nr, ncol = nc)
    colnames(mat) <- cnames
  }

  xmat <- xts(mat, order.by = order.by)

  if(!is.na(rowfill[[1]])) {
    rowfill   <- recycle(rowfill, ncol(xmat))
    xmat[]    <- t(apply(xmat, 1, function(x) rowfill))

  }

  return(xmat)
}

