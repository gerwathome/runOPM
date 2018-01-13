#' @title Read an Eclipse style summary file into R and save it as csv file
#' @description This function converts Eclipse style xy type output to a csv file for later use.
#' @param casename The deck basename of an Eclipse style simulation summary output.  A perl style regular expression may be used to find multiple summary files.  Default behavior is to search recursively in the basedir to find a list of summary files.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".
#' @details The ability to parse the summary files resides in libecl, created by Statoil for use in their Ensemble Reservoir Tool.  This function currently uses the Python wrappers available for libecl, because I haven't yet learned how to use the library directly.
#' @return The function writes out a csv file for each input summary file, into  the appropriate simulation run output directory.  It also appends the new summary data to a csv file with the combined results of all previous runs (in the REPORTS sub directory), and returns a dataframe with the combined summary data.  When the casename duplicates a casename from a previous combined results, the new case is substituted for the old case.
#' @references \href{http://ert.nr.no/ert/index.php/Main_Page}{Ensemble Reservoir Tool}, \href{https://github.com/Statoil/libecl}{libecl}
#' @export
#------------------------------------------------------------------------------
# this is slow; consider how to speed it up
EclSum <- function(casename = "^.+", basedir = "tmp"){
  basedir <- .CheckBasedir(basedir)
  sumfiles <- .FindSummary(basedir = basedir, casename = casename)
  long <- .LongDefinition()
  projsum <- file.path(basedir,"REPORTS","PROJSUM.csv")
  if (file.exists(projsum)) {
    # use readr here because this file may get very large
    long <- readr::read_csv(projsum, col_types = .LongColSpec())
    }
  if (length(sumfiles) > 0) {
    for (infile in sumfiles) {
      case <- basename(infile)
      case <- sub("[.][^.]*$", "", case, perl = TRUE)
      outfile <- file.path(basedir, "OUTPUT", case, paste0(case,"_alt.csv"))
      wide_raw <- .GetECL(case, infile, outfile)
      if (is.data.frame(wide_raw)) {
         wide <- .CleanWide(case, wide_raw)
        # wide <- .AddWOR(wide)
        # wide <- .AddGOR(wide)
        dupcase <- long$CASENAME == case
        long <- long[!dupcase,]
        long <- rbind(long, .Wide2Long(wide))
        readr::write_csv(long, projsum)
       }else{warning("Failed to parse ", infile)}
    } # end for loop
  } else {
    warning(paste0("Failed to find and summary files in directory ",
                   basedir, "for case '", casename, "'"))
  } # end if else sumfiles > 0
  return(long)
}
#------------------------------------------------------------------------------
# keep all of the python in one function, for later removal (I hope)
.GetECL <- function(case, infile, outfile){
  rPython::python.exec("import sys")
  rPython::python.exec("import ecl.ecl as ecl")
#  rPython::python.exec("import ert.ecl.ecl as ecl")
  rPython::python.assign("case",case)
  rPython::python.assign("infile",infile)
  rPython::python.assign("outfile",outfile)
  # the python structure is too complex to read in directly with RJSONIO
  try_err <- try(rPython::python.exec("sum_data = ecl.EclSum(infile)"),
                 silent = TRUE)
  # Using ; as a separator is important here, as some of the keys have
  #  commas in them
  if (is.null(try_err)) {
    rPython::python.exec("sum_data.exportCSV(outfile,date_format = '%d-%b-%Y',sep = ';')")
    rPython::python.assign("sum_data","None")
    wide_raw <- utils::read.csv(file = outfile, sep = ";",
                                stringsAsFactors = FALSE)
  } else {
    warning(paste0("Failed to convert ECL to csv for case ", case))
  }
  return(wide_raw)
}
#------------------------------------------------------------------------------
.CleanWide <- function(case, wide_raw){
#  wide <- wide_raw[, colSums(wide_raw != 0, na.rm = TRUE) > 0]
  wide <- wide_raw
  grep("F(...)", colnames(wide), perl = TRUE)
  colnames(wide) <- sub("F(...)", "W\\1.FIELD", colnames(wide),
                        perl = TRUE)
  inxyzpat <- "(\\d+)\\.(\\d+)\\.(\\d+)"
  outxyzpat <- "\\1_\\2_\\3"
  colnames(wide) <- gsub(inxyzpat, outxyzpat, colnames(wide),
                         perl = TRUE)
  # the output date will be default R format of "%d-%b-%Y"
  wide$DATE <- as.Date(wide$DATE, "%d-%b-%Y")
  wide <- data.frame(CASE = rep(case, length(wide$DATE)), wide,
                     stringsAsFactors = FALSE)
  return(wide)
}
#------------------------------------------------------------------------------
.Wide2Long <- function(wide){
  pat <- "^(\\w+)\\.(\\w+)$";
  #  pat <- "^(\\w+):(\\w+)$";
  vars <-  colnames(wide)
  kw.wgn <- vars[grep(pat, vars, perl = TRUE)]
  kw <- sub(pat, "\\1", kw.wgn, perl = TRUE)
  wgn <- sub(pat, "\\2", kw.wgn, perl = TRUE)
  ndays <- length(wide$DATE)
  long <- .LongDefinition()
  for (i in 1:length(wgn)) {
    tlong <- .LongDefinition(ndays)
    tlong$CASENAME <- wide[,"CASE"]
    tlong$DAYS <- wide[,"DAYS"]
    tlong$DATE <- wide[,"DATE"]
    tlong$WGNAME <- rep(wgn[i],ndays)
    tlong$KEYWORD <- rep(kw[i],ndays)
    tlong$VALUE <- wide[,kw.wgn[i]]
    tlong$UNITS <- rep(.KW2Units(kw[i]),ndays)
    tlong$COMMENT <- ""
    long <- rbind(long,tlong)
  }
  long <- long[!is.na(long$VALUE),]
  return(long)
}
#------------------------------------------------------------------------------
#' @title Find a set of Eclipse style input data decks
#' @description The function returns a list of .DATA files with the full path.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @param casename The deck basename of an Eclipse style simulation summary output.  A perl style regular expression may be used to find multiple summary files.
#' @param ext A list file extents used to create a search pattern.  Default is c(".data", ".DATA")
#' @param recursive Should we look in the directories recursively?  Default is TRUE.
#' @param ... Any other list.files arguments that might be necessary
#' @details Default behavior to to search recursively in the basedir to find a list of ECl style input decks files.
#' @return The function returns a list of .DATA files with the full path.
.FindDecks <- function(basedir = NULL,
                       casename = "^.+",
                       ext = "\\.DATA$",
                       recursive = TRUE,
                       ...){
  if (is.null(basedir) | !dir.exists(basedir)) {
    basedir <- getwd()
  } else {
    basedir <- normalizePath(basedir)
  }
  decks <- character()
  for (pat in ext) {
    pattern <- paste0(casename, pat)
    deckpath <- list.files(path = basedir,
                           pattern = pattern,
                           full.names = TRUE,
                           recursive = recursive,
                           ignore.case = TRUE,
                           include.dirs = TRUE)
    decks <- c(decks, deckpath)
  }
  return(decks)
}
#------------------------------------------------------------------------------
# ecl summary file suffixes (may be either case)
# unformatted unified: .UNSMRY
# unformatted not unified: .Sxxxx
# formatted unified: .FUNSMRY
# formatted not unified: .Axxxx
# look for only unified files, for now
.FindSummary <- function(basedir = "tmp",
                         casename = "^.+",
                         recursive = TRUE,
                         ...){
   ext <- c("\\.F*UNSMRY")
  #  ext <- c(".unsmry", ".UNSMRY", ".funsmry", ".FUNSMRY")
  # basedir <- .CheckBasedir(basedir)
  sumfiles <- .FindDecks(basedir = basedir,
                         casename = casename,
                         ext = ext,
                         recursive = recursive)
  if (length(sumfiles) < 1) {warning(paste0("Failed to locate Eclipse style",
                                            " summary output files. Did the ",
                                            "run '", casename,  "' complete ",
                                            " properly?"))
  }
  return(sumfiles)
}
#------------------------------------------------------------------------------
# this isn't called directly, but used by add_gor and add_wor
.WGNames <- function(df){
  pat <- "^\\w+\\.(\\w+)$";
  #  pat <- "^\\w+:(\\w+)$";
  kw.wgn <- colnames(df)[grep(pat, colnames(df), perl = TRUE)]
  wgn <- sub(pat, "\\1", kw.wgn, perl = TRUE)
  return(unique(wgn))
}
#------------------------------------------------------------------------------
.AddGOR <- function(df){
  wells <- .WGNames(df)
  vars <- colnames(df)
  for (well in wells) {
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed = TRUE)
    gpr <- paste0("WGPR.",well)
    gprcol <- grep(gpr, vars, fixed = TRUE)
    gor <- paste0("WGOR.",well)
    if (any(oprcol) & any(gprcol)) {
      tmp <- df[,gprcol]/df[,oprcol]
      tmp[df[,oprcol] == 0] <- 0
      df <- cbind(df,tmp)
      colnames(df) <- c(vars,gor)
    }
  }
  return(df)
}
#------------------------------------------------------------------------------
.AddWOR <- function(df){
  wells <- .WGNames(df)
  vars <- colnames(df)
  for (well in wells) {
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed = TRUE)
    wpr <- paste0("WWPR.",well)
    wprcol <- grep(wpr, vars, fixed = TRUE)
    wor <- paste0("WWOR.",well)
    if (any(oprcol) & any(wprcol)) {
      tmp <- df[,wprcol]/df[,oprcol]
      tmp[df[,oprcol] == 0] <- 0
      df <- cbind(df,tmp)
      colnames(df) <- c(vars,wor)
    }
  }
  return(df)
}
#------------------------------------------------------------------------------
# functionality to check deck for units type needs to be tested
# still need to see what the Metric units are
# also need to add many other keywords
.KW2Units <- function(keyword, type = "FIELD", deck = NULL){
  if (!is.null(deck)) {type <- .FindUnitType(deck)}
  unitsF <- switch(keyword,
                   "WOPR" = "STBD",      # Oil Prod Rate
                   "WWPR" = "STBD",      # Water Prod Rate
                   "WGPR" = "MSCFD",     # Gas Prod Rate
                   "WOPT" = "MSTB",      # Cum Oil Prod
                   "WWPT" = "MSTB",      # Cum Water Prod
                   "WGPT" = "MMSCF",     # Cum Gas Prod
                   "WGOR" = "MSCF/STB",   # Gas Oil Ratio
                   "WWOR" = "STB/STB",   # Water Oil Ratio
                   "WOIR" = "STBD",      # Oil Inj Rate
                   "WWIR" = "STBD",      # Water Inj Rate
                   "WGIR" = "MSCFD",     # Gas Inj Rate
                   "WOIT" = "MSTB",      # Cum  Oil Inj
                   "WWIT" = "MSTB",      # Cum  Water Inj
                   "WGIT" = "MMSCF",     # Cum  Gas Inj
                   "WBHP" = "PSIA",      # Bottom Hole Pressure
                   "WBDP" = "PSIA",      # well bore pressure drop
                   "BPR" = "PSIA",       # block pressure
                   "BWSAT" = "FRAC",     # block water saturation
                   ""
  )
  unitsM <- switch(keyword,
                   "WOPR" = "STBD",      # Oil Prod Rate
                   "WWPR" = "STBD",      # Water Prod Rate
                   "WGPR" = "MSCFD",     # Gas Prod Rate
                   "WOPT" = "MSTB",      # Cum Oil Prod
                   "WWPT" = "MSTB",      # Cum Water Prod
                   "WGPT" = "MMSCF",     # Cum Gas Prod
                   "WGOR" = "MSCF/STB",   # Gas Oil Ratio
                   "WWOR" = "STB/STB",   # Water Oil Ratio
                   "WOIR" = "STBD",      # Oil Inj Rate
                   "WWIR" = "STBD",      # Water Inj Rate
                   "WGIR" = "MSCFD",     # Gas Inj Rate
                   "WOIT" = "MSTB",      # Cum  Oil Inj
                   "WWIT" = "MSTB",      # Cum  Water Inj
                   "WGIT" = "MMSCF",     # Cum  Gas Inj
                   "WBHP" = "PSIA",      # Bottom Hole Pressure
                   "WBDP" = "PSIA",      # well bore pressure drop
                   "BPR" = "PSIA",       # block pressure
                   "BWSAT" = "FRAC",     # block water saturation
                   ""
  )
  units <- ifelse(type == "FIELD", unitsF, unitsM)
  return(units)
}
#------------------------------------------------------------------------------
.FindUnitType <- function(deck){
  if (!file.exists(deck)) {warning(paste0("Can't find deck to check units ",
                                          "type.  ", "FIELD units are ",
                                          "assumed"))}
  type <- ifelse(
    purrr::is_empty(grep('^METRIC$',readLines(deck))),
    "FIELD",
    "METRIC")
  return(type)
}
#------------------------------------------------------------------------------
.KW2Descrip <- function(keyword){
  descrip <- switch(keyword,
                    "WOPR" = "Oil Prod Rate",
                    "WWPR" = "Water Prod Rate",
                    "WGPR" = "Gas Prod Rate",
                    "WOPT" = "Cum Oil Prod",
                    "WWPT" = "Cum Water Prod",
                    "WGPT" = "Cum Gas Prod",
                    "WGOR" = "Gas Oil Ratio",
                    "WWOR" = "Water Oil Ratio",
                    "WOIR" = "Oil Inj Rate",
                    "WWIR" = "Water Inj Rate",
                    "WGIR" = "Gas Inj Rate",
                    "WOIT" = "Cum Oil Inj",
                    "WWIT" = "Cum Water Inj",
                    "WGIT" = "Cum Gas Inj",
                    "WBHP" = "Bottom Hole Pressure",
                    "WBDP" = "Well Bore Pressure Drop",
                    "BPR"  = "Block Pressure",
                    "BWSAT"  = "Block Water Saturation",
                    "Unknown Parameter"
  )
  return(descrip)
}
#------------------------------------------------------------------------------
# the ... is to pass a type or deck argument to .KW2Units
.KW2Label <- function(keyword, ...){
  label <- paste0(.KW2Descrip(keyword), ", ", .KW2Units(keyword))
  if (identical(.KW2Descrip(keyword),"Unknown Parameter")) {label <- keyword}
  return(label)
}
#------------------------------------------------------------------------------
.WGN_KW2Title <- function(wgn, keyword){
  beg <- wgn
  end <- .KW2Descrip(keyword)
  if (identical(end,"Unknown Parameter")) {end <- keyword}
  title <- paste0(beg, ":  ", end)
  return(title)
}
#------------------------------------------------------------------------------
.LongDefinition <- function(n = 0){
  long <- data.frame(
    CASENAME = character(n),
    DAYS = numeric(n),
    DATE = as.Date(character(n), "%Y-%m-%d"),
    WGNAME = character(n),
    KEYWORD = character(n),
    VALUE = numeric(n),
    UNITS = character(n),
    COMMENT = character(n),
    ERR = numeric(n),
    FRAC_ERR = numeric(n),
    stringsAsFactors = FALSE)
  return(long)
}
#------------------------------------------------------------------------------
.LongColSpec <- function(){
  colspec <- readr::cols(
    CASENAME = readr::col_character(),
    DAYS = readr::col_double(),
    DATE = readr::col_date(format = ""),
    WGNAME = readr::col_character(),
    KEYWORD = readr::col_character(),
    VALUE = readr::col_double(),
    UNITS = readr::col_character(),
    COMMENT = readr::col_character(),
    ERR = readr::col_double(),
    FRAC_ERR = readr::col_double()
  )
  return(colspec)
}
