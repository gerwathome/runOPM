#' @title Import production history
#' @description Import production history into a long format data frame comparable to the way simulation output is stored.
#' @param ssheetfile The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details The spreadsheet file should include a single sheet that starts with a DATE column, followed by desired data. The DATE should be in "%d-%b-%Y" format, e.g. 01-Jan-2017.  The date format requirement will be relaxed when I have time.  The first row should be Eclipse style names with the format Keyword:Name, as WBHP:PROD1.  Optionally, a DAYS column may be added.  DAYS is the number of days since the start of the simulatiion, and the default is NA.  The case name assigned to this data will be the basename of the input file, without the extent.
#' @return A long format data frame with production data.
#' @export
#------------------------------------------------------------------------------
ImportHist <- function(ssheetfile = NULL, basedir="."){
  if (!file.exists(ssheetfile)) {stop(paste("An Excel or csv file must be",
                                            "specified with historical data."))}
  # determine ssheet filetype and import method
  # call .wide2long and add to projsum
  fn <- basename(ssheetfile)
  extent <- gsub("^\\w+\\.", "", fn, perl = TRUE)
  case <- gsub("\\.\\w+$", "", fn, perl = TRUE)
  wide_raw <- data.frame()
  if (any(grepl("csv", extent, perl = TRUE))) {
    wide_raw <- suppressMessages(readr::read_csv(ssheetfile))
    wide_raw$DATE <- format(as.Date(wide_raw$DATE, "%d-%b-%Y"), "%d-%b-%Y")
  } else if (any(grepl("xls", extent, perl = TRUE))) {
    wide_raw <- suppressMessages(readxl::read_excel(ssheetfile))
    wide_raw$DATE <-  format(as.Date(wide_raw$DATE - 1, origin = "1899-12-31"),
                              "%d-%b-%Y")
  } else {stop(paste("Failed to identify input file type of", ssheetfile))}
  vars <- colnames(wide_raw)
  numeric_cols <- !grepl("DAYS|DATE|CASE", vars, perl = TRUE)
  wide_raw[,numeric_cols] <- as.data.frame(
    data.matrix(wide_raw[,numeric_cols]))
  if (!any(grepl("DAYS", vars, perl = TRUE))) {
    wide_raw <- data.frame(DAYS = rep(NA, length(wide_raw$DATE)), wide_raw,
                             stringsAsFactors = FALSE)
  }
  long <- data.frame(
    CASENAME = character(),
    DAYS = numeric(),
    DATE = as.Date(character()),
    WGNAME = character(),
    KEYWORD = character(),
    VALUE = numeric(),
    UNITS = character(),
    COMMENT = character(),
    stringsAsFactors = FALSE)
  projsumfn <- file.path(normalizePath(basedir), "REPORTS", "PROJSUM.csv")
  if (file.exists(projsumfn)) {
    # use readr here because this file may get very large
    long <- readr::read_csv(projsumfn, col_types = readr::cols())
  }
  # append history data to projsum here
  wide <- .CleanWide(case, wide_raw)
  dupcase <- long$CASENAME == case
  long <- long[!dupcase,]
  long <- rbind(long, .Wide2Long(wide))
  readr::write_csv(long, projsumfn)
  return(long)
} # end function
#==============================================================================
#' @title Calculate the differences between the base case and the simulation runs
#' @description blah
#' @param hmvars The hmvars object used to create the runs.
#' @param basecase The case against which all others will be compared.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details There are numerous ways to judge how well the model fits the data.  We are going to assume that a good solution has four characteristics:  first, the mean normalized absolute error is minimized; second, the mean normalized error is minimized; third, the errors are normally distributed; and fourth, the errors have no trend with time.
#'
#' First it is necessary to define the timesteps at which errors are going to be estimated.   The default dates will be those of the base case, and all other case values will need to be available at those dates (or interpolated to those dates, eventually).
#'
#' Next the difference between a case and the base will be called error.  The errors for each Well/Group, Keyword, and Date over all of the cases run will be fit to a normal distribution.  The P10 and P90 values will be used to normalize the errors.
#'
#' It is not absolute clear which populations should be used for picking high and low values for normalization.  Oil rate error might be best normalized over the entire history, but cumulative oil production would be expected to have larger absolute errors later in the field life.  The quality of some types of measurement vary with the absolute value.  For example, water cut value estimates may have significantly larger errors at higher absolute values.  This is because water production tends to slug when water becomes the continuous phase in the wellbore, and an accurate measurement requires a longer period of time.  Operations people tend to frown on longer tests, because it often leads to lower overall production.
#'
#' The quality statistics will be calculated using the normalized errors.  The shapiro.test will be used for normality assessment.  The slope of a linear model with time will be used for trend assessment.  The mean absolute and relative errors will be caculated as expected.
#' @return blah
#' @export
#------------------------------------------------------------------------------
CalcErrs <- function(hmvars = NULL, basecase = NULL, basedir = "."){
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
.GetBaseVals <- function(long,basecase){

}
