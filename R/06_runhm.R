#' @title blah
#' @description blah
#' @param ssheetfile blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
ImportHist <- function(ssheetfile = NULL, basedir="."){
  if (!file.exists(ssheetfile)) {stop(paste("An Excel or csv file must be",
                                            "specified with historical data."))}
  # determine ssheet filetype and import method
  # call .wide2long and add to projsum
  projsumfn <- file.path(normalizePath(basedir), "REPORTS", "PROJSUM.csv")
  projsum <- utils::read.csv(projsumfn)
  return(projsum)
} # end function
#==============================================================================
#' @title blah
#' @description blah
#' @param template blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
CalcErrs <- function(template = NULL, basedir="."){
  hmerrs <- 1
  return(hmerrs)
} # end function
#==============================================================================
#' @title blah
#' @description blah
#' @param template blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
VarSens1 <- function(template = NULL, basedir="."){
  varsens1 <- 1
  return(varsens1)
} # end function
#==============================================================================
#' @title blah
#' @description blah
#' @param template blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
VarSens2 <- function(template = NULL, basedir="."){
  varsens2 <- 1
  return(varsens2)
} # end function
#==============================================================================
