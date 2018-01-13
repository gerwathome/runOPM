#' @title Import production history
#' @description Import production history into a long format data frame comparable to the way simulation output is stored.
#' @param ssheetfile The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is necessary for reading and writing the PROJSUm.csv file, where the history and simulation data is stored.
#' @param format The date format in the csv file being imported.
#' @details The spreadsheet file should include a single sheet that starts with a DATE column, followed by desired data. The DATE should be in "%d-%b-%Y" format, e.g. 01-Jan-2017.  The date format requirement will be relaxed when I have time.  The first row should be Eclipse style names with the format Keyword:Name, as WBHP:PROD1.  Optionally, a DAYS column may be added.  DAYS is the number of days since the start of the simulatiion, and the default is NA.  The case name assigned to this data will be the basename of the input file, without the extent.
#' @return A long format data frame with production data.
#' @export
#------------------------------------------------------------------------------
ImportHist <- function(ssheetfile = NULL, basedir = "tmp", format = "%d-%b-%Y"){
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
  basedir <- .CheckBasedir(basedir)
  projsumfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
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
#' @param long A long format data frame with historical data and the simulation results, populated by the ImportHist and the EclSum functions.
#' @param basecase The case against which all others will be compared.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".
#' @details First it is necessary to define the timesteps at which errors are going to be estimated.   The default dates will be those of the base case, and all other case values will need to be available at those dates (or interpolated to those dates, eventually).
#'
#' The difference between a case and the base will be called ERR. The ERR divided by the base value will be called FRAC_ERR.
#' @return This returns a long format data frame with production data and the calculated errors.
#' @export
#------------------------------------------------------------------------------
CalcErrors <- function(long = NULL, basecase = NULL, basedir = "tmp"){
  # check inputs
  basedir <- .CheckBasedir(basedir)
  decksdir <-  file.path(basedir, "DECKS")
  reportsdir <-  file.path(basedir, "REPORTS")
  projsum <- file.path(reportsdir, "PROJSUM.csv")
  if (is.null(long)) {
    if (file.exists(projsum)) {
      long <- readr::read_csv(projsum, col_types = .LongColSpec())
    } else {stop("Failed to find historical and simulation data")}
  }
  cases <- unique(long$CASENAME)
  if (!any(grepl(basecase, cases, fixed = TRUE))) {
    stop(paste0("Failed to locate the base case ", basecase))
  }
  # calc errors as a vector
  long_base <- long[long$CASENAME == basecase,
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
  # or if both values don't exist
  filt[is.na(filt)] <- FALSE
  long_tmp$ERR[filt] <- long_tmp$VALUE[filt] - long_tmp$BASE_VALUE[filt]
  long_tmp$FRAC_ERR[filt] <- long_tmp$ERR[filt] / long_tmp$BASE_VALUE[filt]
  col_filt <- !grepl("BASE_VALUE", colnames(long_tmp), fixed = TRUE)
  long <- long_tmp[, col_filt]
  # write out results
  readr::write_csv(long, projsum)
  return(long)
} # end function
#==============================================================================
#' @title Summarize errors by member
#' @description A member is defined as the data associated with a unique grouping of all cases, well/group names and keywords.  Summary statistics are calculated for each member.
#' @param long The full path to a .csv, .xls, or .xlsx file containing production history data.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is necessary for saving the results.
#' @param skip_cases It may be inappropriate to include some cases in the error statistics.  The default is to skip all cases with HIST in the name.  This parameter may be a character vector and/or a case insensitive regular expression.
#' @param wgnames A list with well/group/field names for which errors need to be calculated.  Default is all WELL/FIELD/GROUP names in the dataframe.
#' @param keywords A list with parameters to be for which errors need to be calculated, e.g. "WOPR".  Default is all keywords in the dataframe.
#' @param errortypes A list with error types to be calculated.  Available error types are c("ABS_MEAN_ERR", "ABS_MEAN_FRAC_ERR", "MAX_ERR", "MAX_FRAC_ERR", "MEAN_ERR", "MEAN_FRAC_ERR", "MIN_ERR", "MIN_FRAC_ERR", "NORM_PROB_ERR", "NORM_PROB_FRAC_ERR", "SLOPE_ERR", "SLOPE_FRAC_ERR")  The default is "ABS_MEAN_FRAC_ERR".
#' @details The statistics calculated for each error(ERR) and error fraction (FRAC_ERR) are minimum, maximum, mean, mean of absolute values, the approximate probability for the Shapiro-Wilk Normality test, and the slope of the errors with respect to date.  The probability associated with the Shapiro-Wilk test is a null hypothesis probability, that is, the smaller it is, the more likely that the tested sample is from a normal distribution.  This approach probably isn't as good as looking at the residuals plot, but it may be helpful when trying to examine the errors for a large number of parameters.
#'
#' The intent of these criteria is to ensure that the quality of the fit to the historical data is comparable to the expected error in a forecast.  Unfortunately, trying to optimize multiple criteria is very compute intensive, so this may be a fool's game.  Perhaps the best approach will be to pick a single criteria for optimization, and then check the other criteria.
#'
#' A 'member' is the set of data associated with a particular CASENAME, WGNAME, and KEYWORD.  The items within a member each have different dates.  The 'member error' is the summary statistic comparing this member to the equivalent data for the base case.
#'
#' An 'element' is the set of data associated with a particular WGNAME, KEYWORD, and ERRORTYPE.  The items within an element each have different casenames.  A  proxy model created from an element will allow one to estimate the value of an error based on the input values to an experimental design.
#' @return Returns a data frame with various summary statistics for each member, and wrties out a csv file in the REPORTS directory.
#' @export
#------------------------------------------------------------------------------
ErrorByMember <- function(long,
                          basedir = "tmp",
                          skip_cases = "HIST",
                          wgnames = NULL,
                          keywords = NULL,
                          errortypes = c("ABS_MEAN_FRAC_ERR")){
  # this is phenomenally slow, and needs to be refactored
  # Need to add ways to select wgn, keyword, and error type
  # Consider creating in long format, rather than wide to
  #  avoid unk|uninit errors
  skip_filt <- rep(FALSE, nrow(long))
  if (!is.null(skip_cases)) {
    skip_filt <- rep(TRUE, nrow(long))
    for (i in 1:length(skip_cases)) {
      skip_filt <- grepl(skip_cases[i], long$CASENAME,
                         perl = TRUE, ignore.case = TRUE) & skip_filt
    }
  }
  skip_filt <- !skip_filt
  members <- unique(long[skip_filt,c("CASENAME", "WGNAME", "KEYWORD")])
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
    member_error$NORM_PROB_ERR[i] <- ifelse(mean(abs(dist_err)) > 0 &
                                            length(dist_date) > 3 &
                                            length(dist_date) < 5000,
                                            stats::shapiro.test(dist_err)$p.value,
                                             1.0)
    member_error$SLOPE_ERR[i] <- stats::lm(dist_err ~
                                             dist_date)$coefficients[2]
    #
    member_error$MIN_FRAC_ERR[i] <- min(dist_frac_err)
    member_error$MAX_FRAC_ERR[i] <- max(dist_frac_err)
    member_error$MEAN_FRAC_ERR[i] <- mean(dist_frac_err)
    member_error$ABS_MEAN_FRAC_ERR[i] <- mean(abs(dist_frac_err))
    member_error$NORM_PROB_FRAC_ERR[i] <- ifelse(mean(abs(dist_frac_err)) > 0 &
                                                   length(dist_date) > 3 &
                                                   length(dist_date) < 5000,
                                   stats::shapiro.test(dist_frac_err)$p.value,
                                          1.0)
    member_error$SLOPE_FRAC_ERR[i] <- stats::lm(dist_frac_err ~
                                                  dist_date)$coefficients[2]
  }
  basedir <- .CheckBasedir(basedir)
  fn <- file.path(basedir, "REPORTS", "MemberErrorLong.csv")
  mem_err_long <- tidyr::gather(member_error, "ERRORTYPE", "VALUE", -1:-3)
  readr::write_csv(mem_err_long, fn)
  return(mem_err_long)
}
#==============================================================================
#' @title blah
#' @description blah
#' @param hmvars blah
#' @param member_error blah
#' @param model_selection blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
ModelSensitivity <- function(hmvars = NULL, member_error = NULL,
                             model_selection = NULL, basedir = "tmp"){
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  reportsdir <- file.path(basedir,"REPORTS")
  if (is.null(hmvars)) {stop("An hmvars object must be specified.")}
  if (is.null(member_error)) {stop("A member_error object must be specified.")}
  if (is.null(model_selection)) {stop(paste0("A model_selection object must",
                                             " be specified."))}
  designColFilt <- .List2Filt(hmvars$designVars$name,
                              colnames(hmvars$expDesignCoded))
  design <- as.data.frame(hmvars$expDesignCoded[,designColFilt])
  objname <- hmvars$template_name
  response <- .Long2WideError(member_error, model_selection)
  varsens <- multisensi::multisensi(design = design, model = response)
  vs_path <- file.path(reportsdir, paste0(objname, "_varsens.rds"))
  saveRDS(object = varsens, file = vs_path)
  return(varsens)
} # end function
# [*] Dimension Reduction
# [*] Analysis + Sensitivity Indices
# Error: cannot allocate vector of size 54.6 Gb
#==============================================================================
#' @title blah
#' @description blah
#' @param hmvars blah
#' @param member_error blah
#' @param model_selection blah
#' @param basedir blah
#' @details blah
#' @return blah
#' @export
#------------------------------------------------------------------------------
SobelSensitivity <- function(hmvars = NULL, member_error = NULL,
                             model_selection = NULL, basedir = "tmp"){
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  reportsdir <- file.path(basedir,"REPORTS")
  if (is.null(hmvars)) {stop("An hmvars object must be specified.")}
  if (is.null(member_error)) {stop("A member_error object must be specified.")}
  if (is.null(model_selection)) {stop(paste0("A model_selection object must",
                                             " be specified."))}
  design <- as.data.frame(hmvars$expDesignCoded)
  objname <- hmvars$template_name
  response <- .Long2WideError(member_error, model_selection)
  varsens <- suppressWarnings(multisensi::multisensi(design = design,
                                                     model = response))
  vs_path <- file.path(reportsdir, paste0(objname, "_varsens.rds"))
  saveRDS(object = varsens, file = vs_path)
  return(varsens)
} # end function
# [*] Dimension Reduction
# [*] Analysis + Sensitivity Indices
# Error: cannot allocate vector of size 54.6 Gb
#==============================================================================
#' @title SelectModels:  Help filter the member error data frame to select data for the desired kriged models
#' @description This function creates a dataframe of desired unique groupings of WGNAMES, KEYWORDS, and ERRORTYPES available in the member error dataframe, and filters to retrieve this data.
#' @param member_error A long format dataframe with member errors as calculated by the ErrorByMember function.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is necessary for saving the results.
#' @param wgnames A list of Well/Group names to be selected from the member error dataframe.
#' @param keywords A list of keywords to be selected from the member error dataframe.
#' @param errortypes A list of error types to be selected from the member error dataframe.
#' @details blah
#' @return The model selection object:
#' \describe{
#'   \item{"model_selection$all_choices"}{"A data frame giving all combinations of WGNAME, KEYWORD, and ERRORTYPE available in the member error dataframe"}
#'   \item{"model_selection$WGNAME"}{"A vector with all of the Well/Group names available in the member error dataframe"}
#'   \item{"model_selection$KEYWORD"}{"A vector with all of the keywords available in the member error dataframe"}
#'   \item{"model_selection$ERRORTYPE"}{"A vector with all of the error types available in the member error dataframe"}
#'   \item{"model_selection$filt"}{"A filter that will retrieve all of the necessary data from the member error dataframe"}
#'   \item{"model_selection$choice"}{"A dataframe showing the "}
#'   \item{"model_selection$kmfilt"}{}
#' }
#' @export

# model_selection$all_choices
# model_selection$WGNAME
# model_selection$KEYWORD
# model_selection$ERRORTYPE
# model_selection$filt
# model_selection$choice
# model_selection$kmfilt

#------------------------------------------------------------------------------
SelectModels <- function(member_error = NULL, basedir = "tmp", wgnames = NULL,
                         keywords = NULL, errortypes = NULL){
  basedir <- .CheckBasedir(basedir)
  report_path <- file.path(basedir, "REPORTS")
  me_path <- file.path(report_path, "MemberErrorLong.csv")
  if (is.null(member_error)) {
    if (file.exists(me_path)) {
      member_error <- readr::read_csv(me_path)
    } else {
      stop(paste0("A member error data.frame must be provided or a",
                  " MemberErrorLong.csv file must be available"))
    }
  }
  filtwg <- filtkw <- filterr <- rep(TRUE, nrow(member_error))
  if (!is.null(wgnames)) {
    filtwg <- .List2Filt(wgnames, member_error$WGNAME)
    }
  if (!is.null(keywords)) {
    filtkw <- .List2Filt(keywords, member_error$KEYWORD)
    }
  if (!is.null(errortypes)) {
    filterr <- .List2Filt(errortypes, member_error$ERRORTYPE)
    }
  filt <- filtwg & filtkw & filterr
  model_selection <- vector("list",7)
  names(model_selection) <- c("all_choices", "WGNAME", "KEYWORD", "ERRORTYPE",
                              "filt", "choice", "kmfilt")
  wke <- c("WGNAME", "KEYWORD", "ERRORTYPE")
  model_selection[["all_choices"]] <- unique(member_error[,wke])
  model_selection[["WGNAME"]] <- unique(member_error[,"WGNAME"])
  model_selection[["KEYWORD"]] <- unique(member_error[,"KEYWORD"])
  model_selection[["ERRORTYPE"]] <- unique(member_error[,"ERRORTYPE"])
  model_selection[["filt"]] <- filt
  model_selection[["choice"]] <- as.data.frame(unique(member_error[filt, wke]))
  rnames <- paste(
    model_selection$choice$WGNAME,
    model_selection$choice$KEYWORD,
    model_selection$choice$ERRORTYPE,
    sep = "."
  )
  rownames(model_selection$choice) <- rnames
  kmfilt <- apply(model_selection$choice, 1, .WKE2Filt,
                  member_error = member_error)
  model_selection[["kmfilt"]] <- data.frame(kmfilt)
  ms_path <- file.path(report_path, "MemberSelection.rds")
  saveRDS(object = model_selection, file = ms_path)
  return(model_selection)
} # end function
#==============================================================================
#' @title BuildKModels:  Build kriged proxy models
#' @description This function creates a kriged model from the experimental design, the simulation results, and the chosen response variables.
#' @param hmvars The hmvars object has the experimental design that was used to create the simulation decks.
#' @param member_error This contains errors calculated from the results of the simulation runs for each design point (i.e. each row) of the experimental design, and is the output of the ErrorByMember function.
#' @param model_selection This is output by the SelectModels function, and contains the filters to be used on the member error data to select the appropriate data for each kriged model.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is necessary for saving the results.
#' @details blah
#' @return Returns a list of kriged models, one model for each of the elements selected in the model selection object
#' @export
#------------------------------------------------------------------------------
# This function uses only the design variables for the current set of runs
# If it is desirable to include all changing variables over multiple runs,
# it will need to be modified.
BuildKModels <- function(hmvars = NULL, member_error = NULL,
                         model_selection = NULL, basedir = "tmp"){
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  reportsdir <- file.path(basedir,"REPORTS")
  tdp <- character()
  if (is.null(hmvars)) {stop("An hmvars object must be specified.")}
  if (is.null(member_error)) {stop("A member_error object must be specified.")}
  if (is.null(model_selection)) {stop("A model_selection object must be specified.")}
  designColFilt <- .List2Filt(hmvars$designVars$name,
                              colnames(hmvars$expDesignCoded))
  design <- hmvars$expDesignCoded[,designColFilt]
  objname <- hmvars$template_name
  kmfilt <- model_selection$kmfilt
  kmodels <- apply(kmfilt, 2, .Filt2KM, design = design,
                          member_error = member_error)
  km_path <- file.path(reportsdir, paste0(objname, "_km.rds"))
  saveRDS(object = kmodels, file = km_path)
  return(kmodels)
} # end function
#==============================================================================
#' @title Sequential multi-objective Expected Improvement Optimization
#' @description This is a wrapper for GPareto::GParetoptim.  It assumes input of a list of kriged models of error for the chosen well/group name and keyword combination.  The GPareto vignette is very useful for understanding how to use this function: \href{https://cran.r-project.org/web/packages/GPareto/vignettes/GPareto_vignette.pdf}{GPareto: An R Package for Gaussian-Process Based Multi-Objective Optimization and Analysis}
#' @param kmodels A list of kriged models of class km.  There should one model for each of the objectives that are to be included in the multi-objective optimization.
#' @param method The chosen optimization method.  Currently only genoud is supported.  Note that by default genoud seeks a maximum.  For a kriged model of error, Max = FALSE is necessary, which is assumed in this wrapper.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is necessary for saving the results.
#' @param nsteps an integer representing the desired number of iterations.
#' @param ... Additional arguments which may be passed to GParetoptim
#' @details blah
#' @return A list with components:
#' par: a data frame representing the additional points visited during the algorithm,
#' values: a data frame representing the response values at the points given in par,
#' nsteps: an integer representing the desired number of iterations (given in argument),
#' lastmodel: a list of objects of class km corresponding to the last kriging models fitted. If a problem occurs during either model updates or criterion maximization, the last working model and corresponding values are returned.
#' @export
#------------------------------------------------------------------------------
RunGPareto <- function(kmodels = NULL, method = "genoud", basedir = "tmp", nsteps = 10, ...){
  if (is.null(kmodels)) {stop("A list of kriged models must be supplied.")}
  nvars <- kmodels[[1]]@d
  ncases <- length(kmodels)
  maxval <- -10^.Machine$double.min.exp
  minval <- 10^.Machine$double.min.exp
  for (i in 1:ncases) {
    maxval <- max(kmodels[[i]]@X, maxval)
    minval <- min(kmodels[[i]]@X, minval)
  }
  opt_gen <- c(method = "genoud", fn = .kriging.mean, nvars = nvars, max = FALSE,
               hard.generation.limit = FALSE)
  opt_pso <- c(method = "psoptim", fn = .kriging.mean, maxit = 20) # doesn't work, yet
  opt_list <- GPareto::GParetoptim(model = kmodels, fn = .kriging.mean.multi,
                                   nsteps = nsteps,
                                   lower = rep(minval, nvars),
                                   upper = rep(maxval, nvars),
                                   reinterpolation = TRUE,
                                   optimcontrol = opt_gen, ml = kmodels)
  objname <- toupper(deparse(substitute(kmodels)))
  objname <- gsub("\\.", "_", objname)
  fn <- paste0(objname, "_opt.rds")
  basedir <- .CheckBasedir(basedir)
  fp <- file.path(basedir, "REPORTS", fn)
  saveRDS(opt_list, file = fp)
  return(opt_list)
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
.ErrorByElemLongDefinition <- function(n = 0){
  error_by_element <- data.frame(WGNAME = character(n),
                                 KEYWORD = character(n),
                                 ERRORTYPE = character(n),
                                 VALUE = numeric(n),
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
.ErrorByMemLongDefinition <- function(n = 0){
  error_by_member <- data.frame(CASENAME = character(n),
                                 WGNAME = character(n),
                                 KEYWORD = character(n),
                                 ERRORTYPE = character(n),
                                 VALUE = numeric(n),
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
.ErrorByElemLongColSpec <- function(){
  colspec <- readr::cols(
    WGNAME = readr::col_character(),
    KEYWORD = readr::col_character(),
    ERRORTYPE = readr::col_character(),
    VALUE = readr::col_double()
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
.ErrorByMemLongColSpec <- function(){
  colspec <- readr::cols(
    CASENAME = readr::col_character(),
    WGNAME = readr::col_character(),
    KEYWORD = readr::col_character(),
    ERRORTYPE = readr::col_character(),
    VALUE = readr::col_double()
  )
  return(colspec)
}
#==============================================================================
.kriging.mean <- function(Xnew, m){
  DiceKriging::predict.km(m, Xnew, "UK", se.compute = FALSE, checkNames = FALSE)$mean
}
#==============================================================================
# Here ml is a list of km models and results is a matrix  with a row for each
# row in the  design (Xnew) and a column for each km model in ml.  Xnew has
# a column for each variable in the km models
.kriging.mean.multi <- function(Xnew, ml){
  nmodels <- length(ml)
  if (is.vector(Xnew)) {
    nvars <- length(Xnew)
    Xnew <- matrix(Xnew, nrow = 1, ncol = nvars)
  }
  ncases <- nrow(Xnew)
  results <- matrix(data = numeric(length = ncases * nmodels),
                    nrow = ncases, ncol = nmodels)
  colnames(results) <- names(ml)
  for (i in 1:nmodels) {
    results[,i] <- .kriging.mean(Xnew, ml[[i]])
  }
  return(results)
}
#==============================================================================
# supply a list of parameters and a column containing the parameters
# function returns a filter to include all rows for the input list
.List2Filt <- function(filt_list, filt_col){
  filt <- rep(FALSE, length(filt_col))
  nfilt <- length(filt_list)
  for (i in 1:nfilt) {
    tmp <- filt_col == filt_list[i]
    filt <- filt | tmp
  }
  return(filt)
}
# filt_list <- c("FIELD", "PRODU2")
# filt_col <- member_error[,"WGNAME"]
# test <- .List2Filt(filt_list, filt_col)
# sum(test)
# [1] 50400
#==============================================================================
# Supply a member error data frame and a list with WGNAMES, KEYWORD, and
#     ERRORTYPE
# Returns a filter to retrieve all cases and dates for the WKE combination
.WKE2Filt <- function(member_error = NULL, wke = c(NULL, NULL, NULL)){
  WGNAME = wke[1]
  KEYWORD = wke[2]
  ERRORTYPE = wke[3]
  filtwg <- filtkw <- filterr <- rep(TRUE, 1)
  if (!is.null(WGNAME)) {
    filtwg <- .List2Filt(WGNAME, member_error$WGNAME)
  }
  if (!is.null(KEYWORD)) {
    filtkw <- .List2Filt(KEYWORD, member_error$KEYWORD)
  }
  if (!is.null(ERRORTYPE)) {
    filterr <- .List2Filt(ERRORTYPE, member_error$ERRORTYPE)
  }
  filt <- filtwg & filtkw & filterr
  return(filt)
}
# WGNAME <- "FIELD"
# KEYWORD <- "WOPR"
# ERRORTYPE <- "ABS_MEAN_FRAC_ERR"
# wke <- c(WGNAME, KEYWORD, ERRORTYPE)
# filt_wke <- .WKE2Filt(member_error, wke)
# sum(filt_wke)
# [1] 300
#==============================================================================
.Filt2KM <- function(kmfilt, member_error, design){
  design <- as.data.frame(design)
  # print(summary(design))
  # nr <- nrow(design)
  # nc <- ncol(design)
  # cl <- class(design)
  # isdf <- is.data.frame(design)
  # print(paste0("is.data.frame:  ",isdf, ", ", cl,":  ", nr, " by ", nc))
  response <- member_error$VALUE[kmfilt]
  # print(summary(response))
  # nr <- NROW(response)
  # nc <- NCOL(response)
  # cl <- class(response)
  # isvec <- is.vector(response)
  # print(paste0("is.vector:  ",isvec, ", ", cl,":  ", nr, " by ", nc))
  kmodel <- DiceKriging::km(design = design, response =  response)
  # print("after")
  return(kmodel)
}
#==============================================================================
.Long2WideError <- function(member_error, model_selection){
  filt_df <- model_selection$kmfilt
  me_val <- member_error$VALUE
  me_case <- member_error$CASENAME
  f <- function(me_val, filt_df){me_val[filt_df]}
  wide_response <- as.data.frame(apply(filt_df, 2, f, me_val = me_val))
  # below here is just to check the error names are correct
  wr_case <- as.data.frame(apply(filt_df, 2, f, me_val = me_case))
  g <- function(x){length(unique(x)) == 1}
  consistent_names <- all(apply(wr_case, 1, g))
  if (consistent_names) {
    rownames(wide_response) <- wr_case[,1]
  } else {stop("Bummer!  Error values are out of order.")}
  return(wide_response)
}
#
#==============================================================================
