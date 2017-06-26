#' @title Import production history
#' @description Import production history into a long format data frame comparable to the way simulation output is stored.
#' @param ssheetfile The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @param format The date format in the csv file being imported.
#' @details The spreadsheet file should include a single sheet that starts with a DATE column, followed by desired data. The DATE should be in "%d-%b-%Y" format, e.g. 01-Jan-2017.  The date format requirement will be relaxed when I have time.  The first row should be Eclipse style names with the format Keyword:Name, as WBHP:PROD1.  Optionally, a DAYS column may be added.  DAYS is the number of days since the start of the simulatiion, and the default is NA.  The case name assigned to this data will be the basename of the input file, without the extent.
#' @return A long format data frame with production data.
#' @export
#------------------------------------------------------------------------------
ImportHist <- function(ssheetfile = NULL, basedir = ".", format = "%d-%b-%Y"){
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
    # .CleanWide expects data format of "%d-%b-%Y", as .GetECL puts out
    wide_raw$DATE <- format(as.Date(wide_raw$DATE, format), "%d-%b-%Y")
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
  long <- .LongDefinition()
  projsumfn <- file.path(normalizePath(basedir), "REPORTS", "PROJSUM.csv")
  if (file.exists(projsumfn)) {
    # use readr here because this file may get very large
    long <- readr::read_csv(projsumfn, col_types = .LongColSpec())
  }
  # append history data to projsum here
  # .CleanWide expects data format of "%d-%b-%Y", as .GetECL puts out
  wide <- .CleanWide(case, wide_raw)
  dupcase <- long$CASENAME == case
  long <- long[!dupcase,]
  long <- rbind(long, .Wide2Long(wide))
  readr::write_csv(long, projsumfn)
  return(long)
} # end function
#==============================================================================
#' @title Calculate the differences between the base case and the simulation runs
#' @description This calculates the difference between a base case and a simulation case, as well as the fractional difference.
#' @param long A long format data frame with historical data and the simulation results.
#' @param base_case The case against which all others will be compared.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details There are numerous ways to judge how well the model fits the data.  We are going to assume that a good solution has four characteristics:  first, the mean normalized absolute error is minimized; second, the mean normalized error is minimized (i.e. close to zero); third, the errors are normally distributed; and fourth, the errors have no trend with time (i.e the slope of a fit through the residuals as a function of time is near zero).  The intent of these criteria is to ensure that the quality of the fit to the historical data is comparable to the expected error in a forecast.
#'
#' First it is necessary to define the timesteps at which errors are going to be estimated.   The default dates will be those of the base case, and all other case values will need to be available at those dates (or interpolated to those dates, eventually).
#'
#' The difference between a case and the base will be called ERR. The ERR divided by the base value will be called FRAC_ERR.
#' @return This returns a long format data frame with production data and the calculated errors.
#' @export
#------------------------------------------------------------------------------
CalcErrors <- function(long = NULL, base_case = NULL, basedir = "."){
  # check inputs
  basedir <-  normalizePath(basedir)
  decksdir <-  file.path(basedir, "DECKS")
  reportsdir <-  file.path(basedir, "REPORTS")
  projsum <- file.path(reportsdir, "PROJSUM.csv")
  if (is.null(long)) {
    if (file.exists(projsum)) {
      long <- readr::read_csv(projsum, col_types = .LongColSpec())
    } else {stop("Failed to find historical and simulation data")}
  }
  cases <- unique(long$CASENAME)
  if (!any(grepl(base_case, cases, fixed = TRUE))) {
    stop(paste0("Failed to locate the base case ", base_case))
  }
# calc errors as a vector
  long_base <- long[long$CASENAME == "HIST_CSV",
                   c("DATE", "WGNAME", "KEYWORD", "VALUE")]
  colnames(long_base) <- sub("VALUE", "BASE_VALUE", colnames(long_base),
                             fixed = TRUE)
  long_tmp <- dplyr::left_join(long, long_base,
                                     by = c("DATE", "WGNAME", "KEYWORD"))
  long_tmp$ERR <- rep(NA, nrow(long_tmp))
  long_tmp$FRAC_ERR <- rep(NA, nrow(long_tmp))
  # we don't wish to calculate error if either value is effectively 0
  tolerance = .Machine$double.eps^0.5
  filt_value <- abs(long_tmp$VALUE - 0) < tolerance
  filt_base_value <- abs(long_tmp$BASE_VALUE - 0) < tolerance
  filt <- !filt_value & !filt_base_value
  long_tmp$ERR[filt] <- long_tmp$VALUE[filt] - long_tmp$BASE_VALUE[filt]
  long_tmp$FRAC_ERR[filt] <- long_tmp$ERR[filt] / long_tmp$BASE_VALUE[filt]
  col_filt <- !grepl("BASE_VALUE", colnames(long_tmp), fixed = TRUE)
  long <- long_tmp[, col_filt]
# write out results
  readr::write_csv(long, projsum)
  return(long)
} # end function
#==============================================================================
#' @title Summarize errors by production element
#' @description A production element is defined as the unique grouping of all well/group names and keywords.  Summary statistics are caculated for each element.
#' @param long The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is necessary for saving the results.
#' @param skip_cases It may be inappropriate to include some cases in the eroor statistics.  The default is to skip all cases with HIST in the name.  This parameter may be a character vector and/or a case insensitive regular expression.
#' @details The statistics calculated for each error(ERR) and error fraction (FRAC_ERR) are minimum, maximum, mean, mean of absolute values, the approximate probability for the Shapiro-Wilk Normality test, and the slope of the errors with respect to date.
#' @return Returns a data frame with various summary statistics for each element, and wrties out a csv file in the REPORTS directory.
#' @export
#------------------------------------------------------------------------------
ErrorByElement <- function(long, basedir = ".", skip_cases = "HIST"){
  skip_filt <- rep(FALSE, nrow(long))
  if (!is.null(skip_cases)) {
    skip_filt <- rep(TRUE, nrow(long))
    for (i in 1:length(skip_cases)) {
      skip_filt <- grepl(skip_cases[i], long$CASENAME,
                         perl = TRUE, ignore.case = TRUE) & skip_filt
    }
  }
  skip_filt <- !skip_filt
  elements <- unique(long[,c("WGNAME", "KEYWORD")])
  element_error <- .ErrorByElementDefinition(nrow(elements))
  element_error[,"WGNAME"] <- elements$WGNAME
  element_error[,"KEYWORD"] <- elements$KEYWORD
  for (i in 1:nrow(elements)) {
    filtwgn <- grepl(element_error[i,"WGNAME"], long$WGNAME, fixed = TRUE)
    filtkw <- grepl(element_error[i,"KEYWORD"], long$KEYWORD, fixed = TRUE)
    filtna <- !is.na(long$ERR)
    filt <- filtwgn & filtkw & filtna & skip_filt
    dist_date <- long[filt,]$DATE
    if (length(dist_date) == 0) {next}
    dist_err <- long[filt,]$ERR
    dist_frac_err <- long[filt,]$FRAC_ERR
    element_error$MIN_ERR[i] <- min(dist_err)
    element_error$MAX_ERR[i] <- max(dist_err)
    element_error$MEAN_ERR[i] <- mean(dist_err)
    element_error$ABS_MEAN_ERR[i] <- mean(abs(dist_err))
    element_error$NORM_PROB_ERR[i] <- ifelse(mean(abs(dist_err)) > 0,
                                        stats::shapiro.test(dist_err)$p.value,
                                             1.0)
    element_error$SLOPE_ERR[i] <- stats::lm(dist_err ~
                                              dist_date)$coefficients[2]
    #
    element_error$MIN_FRAC_ERR[i] <- min(dist_frac_err)
    element_error$MAX_FRAC_ERR[i] <- max(dist_frac_err)
    element_error$MEAN_FRAC_ERR[i] <- mean(dist_frac_err)
    element_error$ABS_MEAN_FRAC_ERR[i] <- mean(abs(dist_frac_err))
    element_error$NORM_PROB_FRAC_ERR[i] <- ifelse(mean(abs(dist_frac_err)) > 0,
                                    stats::shapiro.test(dist_frac_err)$p.value,
                                                  1.0)
    element_error$SLOPE_FRAC_ERR[i] <- stats::lm(dist_frac_err
                                          ~ dist_date)$coefficients[2]
  }
  fn <- file.path(normalizePath(basedir), "REPORTS", "ElementError.csv")
  readr::write_csv(element_error, fn)
  return(element_error)
}
#==============================================================================
#' @title Summarize errors by member
#' @description A production member is defined as the unique grouping of all cases, well/group names and keywords.  Summary statistics are calculated for each member.
#' @param long The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is necessary for saving the results.
#' @param skip_cases It may be inappropriate to include some cases in the eroor statistics.  The default is to skip all cases with HIST in the name.  This parameter may be a character vector and/or a case insensitive regular expression.
#' @details The statistics calculated for each error(ERR) and error fraction (FRAC_ERR) are minimum, maximum, mean, mean of absolute values, the approximate probability for the Shapiro-Wilk Normality test, and the slope of the errors with respect to date.
#' @return Returns a data frame with various summary statistics for each member, and wrties out a csv file in the REPORTS directory.
#' @export
#------------------------------------------------------------------------------
ErrorByMember <- function(long, basedir = ".", skip_cases = "HIST"){
  skip_filt <- rep(FALSE, nrow(long))
  if (!is.null(skip_cases)) {
    skip_filt <- rep(TRUE, nrow(long))
    for (i in 1:length(skip_cases)) {
      skip_filt <- grepl(skip_cases[i], long$CASENAME,
                         perl = TRUE, ignore.case = TRUE) & skip_filt
    }
  }
  skip_filt <- !skip_filt
  members <- unique(long[,c("CASENAME", "WGNAME", "KEYWORD")])
  member_error <- .ErrorByMemberDefinition(nrow(members))
  member_error[,"CASENAME"] <- members$CASENAME
  member_error[,"WGNAME"] <- members$WGNAME
  member_error[,"KEYWORD"] <- members$KEYWORD
  for (i in 1:nrow(members)) {
    filtcase <- grepl(member_error[i,"CASENAME"], long$CASENAME, fixed = TRUE)
    filtwgn <- grepl(member_error[i,"WGNAME"], long$WGNAME, fixed = TRUE)
    filtkw <- grepl(member_error[i,"KEYWORD"], long$KEYWORD, fixed = TRUE)
    filtna <- !is.na(long$ERR)
    filt <- filtcase & filtwgn & filtkw & filtna & skip_filt
    dist_date <- long[filt,]$DATE
    if (length(dist_date) == 0) {next}
    dist_err <- long[filt,]$ERR
    dist_frac_err <- long[filt,]$FRAC_ERR
    member_error$MIN_ERR[i] <- min(dist_err)
    member_error$MAX_ERR[i] <- max(dist_err)
    member_error$MEAN_ERR[i] <- mean(dist_err)
    member_error$ABS_MEAN_ERR[i] <- mean(abs(dist_err))
    member_error$NORM_PROB_ERR[i] <- ifelse(mean(abs(dist_err)) > 0,
                                     stats::shapiro.test(dist_err)$p.value,
                                             1.0)
    member_error$SLOPE_ERR[i] <- stats::lm(dist_err ~
                                             dist_date)$coefficients[2]
    #
    member_error$MIN_FRAC_ERR[i] <- min(dist_frac_err)
    member_error$MAX_FRAC_ERR[i] <- max(dist_frac_err)
    member_error$MEAN_FRAC_ERR[i] <- mean(dist_frac_err)
    member_error$ABS_MEAN_FRAC_ERR[i] <- mean(abs(dist_frac_err))
    member_error$NORM_PROB_FRAC_ERR[i] <- ifelse(mean(abs(dist_frac_err)) > 0,
                                   stats::shapiro.test(dist_frac_err)$p.value,
                                          1.0)
    member_error$SLOPE_FRAC_ERR[i] <- stats::lm(dist_frac_err ~
                                                  dist_date)$coefficients[2]
  }
  fn <- file.path(normalizePath(basedir), "REPORTS", "MemberError.csv")
  readr::write_csv(member_error, fn)
  return(member_error)
}
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
.ErrorByElementDefinition <- function(n = 0){
  error_by_element <- data.frame(WGNAME = character(n),
                                 KEYWORD = character(n),
                                 MIN_ERR = numeric(n),
                                 MAX_ERR = numeric(n),
                                 MEAN_ERR = numeric(n),
                                 ABS_MEAN_ERR = numeric(n),
                                 NORM_PROB_ERR = numeric(n),
                                 SLOPE_ERR = numeric(n),
                                 MIN_FRAC_ERR = numeric(n),
                                 MAX_FRAC_ERR = numeric(n),
                                 MEAN_FRAC_ERR = numeric(n),
                                 ABS_MEAN_FRAC_ERR = numeric(n),
                                 NORM_PROB_FRAC_ERR = numeric(n),
                                 SLOPE_FRAC_ERR = numeric(n),
                                 stringsAsFactors = FALSE)
  return(error_by_element)
}
#==============================================================================
.ErrorByMemberDefinition <- function(n = 0){
  error_by_member <- data.frame(CASENAME = character(n),
                                WGNAME = character(n),
                                KEYWORD = character(n),
                                MIN_ERR = numeric(n),
                                MAX_ERR = numeric(n),
                                MEAN_ERR = numeric(n),
                                ABS_MEAN_ERR = numeric(n),
                                NORM_PROB_ERR = numeric(n),
                                SLOPE_ERR = numeric(n),
                                MIN_FRAC_ERR = numeric(n),
                                MAX_FRAC_ERR = numeric(n),
                                MEAN_FRAC_ERR = numeric(n),
                                ABS_MEAN_FRAC_ERR = numeric(n),
                                NORM_PROB_FRAC_ERR = numeric(n),
                                SLOPE_FRAC_ERR = numeric(n),
                                stringsAsFactors = FALSE)
  return(error_by_member)
}
#==============================================================================
.ErrorByElementColSpec <- function(){
  colspec <- readr::cols(
    WGNAME = readr::col_character(),
    KEYWORD = readr::col_character(),
    MIN_ERR = readr::col_double(),
    MAX_ERR = readr::col_double(),
    MEAN_ERR = readr::col_double(),
    ABS_MEAN_ERR = readr::col_double(),
    NORM_PROB_ERR = readr::col_double(),
    SLOPE_ERR = readr::col_double(),
    MIN_FRAC_ERR = readr::col_double(),
    MAX_FRAC_ERR = readr::col_double(),
    MEAN_FRAC_ERR = readr::col_double(),
    ABS_MEAN_FRAC_ERR = readr::col_double(),
    NORM_PROB_FRAC_ERR = readr::col_double(),
    SLOPE_FRAC_ERR = readr::col_double()
  )
  return(colspec)
}
#==============================================================================
.ErrorByMemberColSpec <- function(){
  colspec <- readr::cols(
    CASENAME = readr::col_character(),
    WGNAME = readr::col_character(),
    KEYWORD = readr::col_character(),
    MIN_ERR = readr::col_double(),
    MAX_ERR = readr::col_double(),
    MEAN_ERR = readr::col_double(),
    ABS_MEAN_ERR = readr::col_double(),
    NORM_PROB_ERR = readr::col_double(),
    SLOPE_ERR = readr::col_double(),
    MIN_FRAC_ERR = readr::col_double(),
    MAX_FRAC_ERR = readr::col_double(),
    MEAN_FRAC_ERR = readr::col_double(),
    ABS_MEAN_FRAC_ERR = readr::col_double(),
    NORM_PROB_FRAC_ERR = readr::col_double(),
    SLOPE_FRAC_ERR = readr::col_double()
  )
  return(colspec)
}
#==============================================================================
