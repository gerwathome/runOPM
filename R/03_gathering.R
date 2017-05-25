#' @title Read an Eclipse style summary file into R and save it as csv file
#' @description This function converts Eclipse style xy type output to a csv file for later use.
#' @param casename The deck basename of an Eclipse style simulation summary output.  A perl style regular expression may be used to find multiple summary files.  Default behavior to to search recursively in the basedir to find a list of summary files.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details The ability to parse the summary files resides in libecl, created by Statoil for use in their Ensemble Reservoir Tool.  This function currently uses the Python wrappers available for libecl, because I haven't yet learned how to use the library directly.
#' @return The function writes out a csv file for each input summary file, into  the appropriate simulation run output directory.  It also appends the new summary data to a csv file with the combined results of all previous runs (in the REPORTS sub directory), and returns a dataframe with the combined summary data.  When the casename duplicates a casename from a previous combined results, the new case is substituted for the old case.
#' @references \href{http://ert.nr.no/ert/index.php/Main_Page}{Ensemble Reservoir Tool}, \href{https://github.com/Statoil/libecl}{libecl}
#' @export
#------------------------------------------------------------------------------
eclsum <- function(casename = "^.+", basedir="."){
  basedir <- normalizePath(basedir)
  sumfiles <- .findSummary(basedir = basedir, casename = casename)
  long <- data.frame(
    CASENAME=character(),
    DAYS=numeric(),
    DATE=as.Date(character()),
    WGNAME=character(),
    KEYWORD=character(),
    VALUE=numeric(),
    UNITS=character(),
    COMMENT=character(),
    stringsAsFactors=FALSE)
  projsum <- file.path(basedir,"REPORTS","PROJSUM.csv")
  if(file.exists(projsum)){long <- readr::read_csv(projsum,
                                         col_types = readr::cols())}
  for(infile in sumfiles){
    case <- basename(infile)
    case <- sub("[.][^.]*$", "", case, perl=TRUE)
    dupcase <- long$CASENAME == case
    long <- long[!dupcase,]
    outfile <- file.path(basedir, "OUTPUT", case, paste0(case,".csv"))
    rPython::python.exec("import sys")
    rPython::python.exec("import ert.ecl.ecl as ecl")
    rPython::python.assign("case",case)
    rPython::python.assign("infile",infile)
    rPython::python.assign("outfile",outfile)
    # the python structure is too complex to read in directly with RJSONIO
    try_err <- try(rPython::python.exec("sum_data = ecl.EclSum(infile)"),
                   silent=TRUE)
    # Using ; as a seperator is important here, as some of the keys have commas
    # in them
    if(is.null(try_err)){
      rPython::python.exec("sum_data.exportCSV(outfile, date_format='%d-%b-%Y',
                           sep=';')")
      rPython::python.assign("sum_data","None")
      wide.raw <- utils::read.csv(file=outfile, sep = ";",
                                 stringsAsFactors = FALSE)
      wide <- wide.raw[, colSums(wide.raw != 0, na.rm = TRUE) > 0]
      grep("F(...)", colnames(wide), perl=TRUE)
      colnames(wide) <- sub("F(...)", "W\\1.FIELD", colnames(wide), perl=TRUE)
      inxyzpat <- "(\\d+)\\.(\\d+)\\.(\\d+)"
      outxyzpat <- "\\1_\\2_\\3"
      colnames(wide) <- gsub(inxyzpat, outxyzpat, colnames(wide), perl=TRUE)
      wide$DATE <- as.Date(wide$DATE, "%d-%b-%Y")
      wide <- data.frame(CASE=rep(case,length(wide$DATE)),wide)
      wide <- .add_wor(wide)
#      wide <- .add_gor(wide)
      long <- rbind(long, .wide2long(wide))
      readr::write_csv(long, projsum)
      return(long)
    }else{warning("Failed to parse ", infile)}
  }
  return(long)
}
#------------------------------------------------------------------------------
.wide2long <- function(df){
  dfl <- data.frame(
    CASENAME=character(),
    DAYS=numeric(),
    DATE=as.Date(character()),
    WGNAME=character(),
    KEYWORD=character(),
    VALUE=numeric(),
    UNITS=character(),
    COMMENT=character(),
    stringsAsFactors=FALSE)
  pat <- "^(\\w+)\\.(\\w+)$";
#  pat <- "^(\\w+):(\\w+)$";
  vars <-  colnames(df)
  kw.wgn <- vars[grep(pat, vars, perl=TRUE)]
  kw <- sub(pat, "\\1", kw.wgn, perl=TRUE)
  wgn <- sub(pat, "\\2", kw.wgn, perl=TRUE)
  ndays <- length(df$DAYS)
  for(i in 1:length(wgn)){
#    if(all(df[,3+i]==0, na.rm = TRUE)){next}
    tdfl <- data.frame(
      CASENAME = df[,"CASE"],
      DAYS = df[,"DAYS"],
      DATE = df[,"DATE"],
      WGNAME = rep(wgn[i],ndays),
      KEYWORD = rep(kw[i],ndays),
      VALUE = df[,kw.wgn[i]],
      UNITS = rep(.kw2units(kw[i]),ndays),
      COMMENT = "",
      stringsAsFactors = FALSE)
    dfl <- rbind(dfl,tdfl)
  }
  dfl <- dfl[!is.na(dfl$VALUE),]
  return(dfl)
}
#------------------------------------------------------------------------------
# ecl summary file suffixes (may be either case)
# unformatted unified: .UNSMRY
# unformatted not unified: .Sxxxx
# formatted unified: .FUNSMRY
# formatted not unified: .Axxxx
# look for only unified files, for now
.findDecks <- function(basedir = ".",
                       casename = "^.+",
                       ext = c(".data", ".DATA"),
                       recursive = TRUE){
  decks <- character()
  for(pat in ext){
    pattern <- paste0(casename, pat)
    searchdirs <- list.dirs(path = basedir,
                           full.names = TRUE,
                           recursive = TRUE)
    searchdir <- searchdirs[grep(casename,searchdirs,perl=TRUE)]
    deckpath <-list.files(path=searchdir,
                          pattern=pattern,
                          full.names = TRUE,
                          recursive=recursive,
                          include.dirs=TRUE)
    decks <- c(decks, deckpath)
  }
  return(decks)
}
#------------------------------------------------------------------------------
.findSummary <- function(basedir = ".",
                         casename = "^.+",
                         recursive = TRUE){
  ext <- c(".unsmry", ".UNSMRY", ".funsmry", ".FUNSMRY")
  sumfiles <- .findDecks(basedir = basedir,
                         casename = casename,
                         ext = ext,
                         recursive = recursive)
  if(length(sumfiles) < 1){stop(paste0("Failed to locate Eclipse style summary",
                                       " output files.  Did the run ", casename,
                                       " complete properly?"))}
  return(sumfiles)
}
#------------------------------------------------------------------------------
# this isn't called directly, but used by add_gor and add_wor
.wgnames <- function(df){
  pat <- "^\\w+\\.(\\w+)$";
#  pat <- "^\\w+:(\\w+)$";
  kw.wgn <- colnames(df)[grep(pat, colnames(df), perl=TRUE)]
  wgn <- sub(pat, "\\1", kw.wgn, perl=TRUE)
  return(unique(wgn))
}
#------------------------------------------------------------------------------
.add_gor <- function(df){
  wells <- .wgnames(df)
  vars <- colnames(df)
  for(well in wells){
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed=TRUE)
    gpr <- paste0("WGPR.",well)
    gprcol <- grep(gpr, vars, fixed=TRUE)
    gor <- paste0("WGOR.",well)
    if(any(oprcol) & any(gprcol)){
      tmp <- df[,gprcol]/df[,oprcol]
      tmp[df[,oprcol]==0] <- 0
      df <- cbind(df,tmp)
      colnames(df) <- c(vars,gor)
    }
  }
  return(df)
}
#------------------------------------------------------------------------------
.add_wor <- function(df){
  wells <- .wgnames(df)
  vars <- colnames(df)
  for(well in wells){
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed=TRUE)
    wpr <- paste0("WWPR.",well)
    wprcol <- grep(wpr, vars, fixed=TRUE)
    wor <- paste0("WWOR.",well)
    if(any(oprcol) & any(wprcol)){
      tmp <- df[,wprcol]/df[,oprcol]
      tmp[df[,oprcol]==0] <- 0
      df <- cbind(df,tmp)
      colnames(df) <- c(vars,wor)
    }
  }
  return(df)
}
#------------------------------------------------------------------------------
# functionality to check deck for units type needs to be tested
# still nned to see what the Metric units are
.kw2units <- function(keyword,type="FIELD", deck=NULL){
  if(!is.null(deck)){type <- .findUnitType(deck)}
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
                   ""
  )
  units <- ifelse(type == "FIELD", unitsF, unitsM)
  return(units)
}
#------------------------------------------------------------------------------
.findUnitType <- function(deck){
  if(!file.exists(deck)){warning(paste("Can't find deck to check units type.",
                                       "FIELD units is assumed", sep=" "))}
  type <- ifelse(
    purrr::is_empty(grep('METRIC',readLines(deck))),
    "FIELD",
    "METRIC")
  return(type)
}
#------------------------------------------------------------------------------
.kw2descrip <- function(keyword){
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
                  "Unknown Parameter"
  )
  return(descrip)
}
#------------------------------------------------------------------------------
.kw2label <- function(keyword, ...){
  label <- paste0(.kw2descrip(keyword), ", ", .kw2units(keyword))
  return(label)
}
#------------------------------------------------------------------------------
