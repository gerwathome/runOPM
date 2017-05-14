#' @title Eclipse style summary file to csv
#' @description This function converts Eclipse style xy type output to a csv file for later use.
#' @param indir The path to an Eclipse style simulation summary output
#' @param outdir The  path to where the csv file with the summary data should be written, assumed to be the same as the indir.
#' @param projdir The  path to where the csv file with summary data for (possibly) multiple wells should be written, assumed to be the same as the indir.
#' @return The function writes out a csv file, and returns a dataframe with summary data.
#------------------------------------------------------------------------------
eclsum <- function(sumdir=".", outdir=NULL, projdir=NULL){
  sumdir <-normalizePath(sumdir)
  if(is.null(outdir)){outdir <- sumdir}
  if(is.null(projdir)){projdir <- sumdir}
  sumfiles <- .findEclSummary(sumdir)
  outdir <-normalizePath(outdir)
  for(infile in sumfiles){
    case <- basename(infile)
    case <- sub("[.][^.]*$", "", case, perl=TRUE)
    outfile <- file.path(outdir, paste0(case,".csv"))
    # library(rPython)
    rPython::python.exec("import sys")
    rPython::python.exec("import ert.ecl.ecl as ecl")
    rPython::python.assign("case",case)

    # need to add some logic to deal with the case when EclSum fails for some
    # reason
    #
    # the python structure is too complex to read in directly with RJSONIO
    try_err <- try(rPython::python.exec("sum_data = ecl.EclSum(infile)"),
                   silent=TRUE)
    if(is.null(try_err)){
      rPython::python.exec("sum_data.exportCSV(outfile, date_format='%d-%b-%Y', sep=',')")
      rPython::python.assign("sum_data","None")
      rslts <- read.csv(file=outfile)
      rslts$DATE <- as.Date(rslts$DATE, "%d-%b-%Y")
      rslts <- data.frame(CASE=rep(case,length(rslts$DATE)),rslts)}
    else{rslts <- data.frame(CASE=character(0), DAYS=numeric(0))}
  }
  return(long)
}

.wide2long <- function(df){
  dfl <- data.frame(
    BASE=character(),
    VARIANT=character(),
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
  kw.wgn <- colnames(df)[grep(pat, colnames(df), perl=TRUE)]
  kw <- sub(pat, "\\1", kw.wgn, perl=TRUE)
  wgn <- sub(pat, "\\2", kw.wgn, perl=TRUE)
  ndays <- length(df$DAYS)
  for(i in 1:length(wgn)){
    if(all(df[,3+i]==0,na.rm = TRUE)){next}
    tdfl <- data.frame(
      BASE=cname2base_var(df[,1])[,"BASE"],
      VARIANT=cname2base_var(df[,1])[,"VARIANT"],
      CASENAME=df[,1],
      DAYS=df[,2],
      DATE=df[,3],
      WGNAME=rep(wgn[i],ndays),
      KEYWORD=rep(kw[i],ndays),
      VALUE=df[,3+i],
      UNITS=rep(.kw2units(kw[i]),ndays),
      COMMENT="",
      stringsAsFactors=FALSE)
    dfl <- rbind(dfl,tdfl)
  }
  dfl <- dfl[!is.na(dfl$VALUE),]
  return(dfl)
}



# ecl summary file suffixes (may be either case)
# unformatted unified: .UNSMRY
# unformatted not unified: .Sxxxx
# formatted unified: .FUNSMRY
# formatted not unified: .Axxxx
# look for only unified files, for now

.findDecks <- function(indir=".", ext=c(".data", ".DATA"), recursive = FALSE){
  decks <- character()
  for(pat in ext){
    deck <- normalizePath(list.files(path=indir,
                                     pattern=paste0("^.+", pat," $"),
                                     full.names = TRUE,
                                     recursive=recursive,
                                     include.dirs=TRUE))
    decks <- c(decks, deck)
  }
  return(decks)
}

.findSummary <- function(indir=".", recursive = FALSE){
  ext <- c(".unsmry", ".UNSMRY", ".funsmry", ".FUNSMRY")
  sumfiles <- .findDecks(indir=indir, ext=ext, recursive = recursive)
  return(sumfiles)
}

#------------------------------------------------------------------------------
# this isn't called directly, but used by add_gor and add_wor
.wgnames <- function(df){
  pat <- "^\\w+\\.(\\w+)$";
  kw.wgn <- colnames(df)[grep(pat, colnames(df), perl=TRUE)]
  wgn <- sub(pat, "\\1", kw.wgn, perl=TRUE)
  return(unique(wgn))
}

#------------------------------------------------------------------------------
.add_gor <- function(df){
  wells <- .wgnames(df)
  for(well in wells){
    vars <- colnames(df)
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed=TRUE)
    gpr <- paste0("WGPR.",well)
    gprcol <- grep(gpr, vars, fixed=TRUE)
    gor <- paste0("WGOR.",well)
    if(oprcol & gprcol){
      tmp <- df[,gprcol]/df[,oprcol]*1000
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
  for(well in wells){
    vars <- colnames(df)
    opr <- paste0("WOPR.",well)
    oprcol <- grep(opr, vars, fixed=TRUE)
    wpr <- paste0("WWPR.",well)
    wprcol <- grep(wpr, vars, fixed=TRUE)
    wor <- paste0("WWOR.",well)
    if(oprcol & wprcol){
      tmp <- df[,wprcol]/df[,oprcol]
      tmp[df[,oprcol]==0] <- 0
      df <- cbind(df,tmp)
      colnames(df) <- c(vars,wor)
    }
  }
  return(df)
}

.kw2units <- function(keyword){
  units <- switch(keyword,
                  "WOPR" = "STBD",      # Oil Prod Rate
                  "WWPR" = "STBD",      # Water Prod Rate
                  "WGPR" = "MSCFD",     # Gas Prod Rate
                  "WOPT" = "MSTB",      # Cum Oil Prod
                  "WWPT" = "MSTB",      # Cum Water Prod
                  "WGPT" = "MMSCF",     # Cum Gas Prod
                  "WGOR" = "SCF/STB",   # Gas Oil Ratio
                  "WWOR" = "STB/STB",   # Water Oil Ratio
                  "WOIR" = "STBD",      # Oil Inj Rate
                  "WWIR" = "STBD",      # Water Inj Rate
                  "WGIR" = "MSCFD",     # Gas Inj Rate
                  "WOIT" = "MSTB",      # Cum  Oil Inj
                  "WWIT" = "MSTB",      # Cum  Water Inj
                  "WGIT" = "MMSCF",     # Cum  Gas Inj
                  "WBHP" = "PSIA",      # Bottom Hole Pressure
                  "WBDP" = "PSIA"       # well bore pressure drop
  )
  return(units)
}

#------------------------------------------------------------------------------
# This function assumes that the casename is made up of a BASE separated
# from a VARIANT by an "splt".  The BASE may have an internal "splt",
# but the VARIANT may not.
.cname2base_var <- function(casename, splt = "_"){
  ncn <- length(casename)
  df <- data.frame(
    BASE=character(),
    VARIANT=character(),
    stringsAsFactors=FALSE)
  for(i in 1:ncn){
    nc <- nchar(casename[i])
    found <- as.vector(gregexpr(splt, casename[i], perl=TRUE)[[1]])
    nf <- length(found)
    if(found[1] < 0){
      temp <- data.frame(
        BASE = casename[i],
        VARIANT = "_",
        stringsAsFactors=FALSE)
      df <- rbind(df, temp)
    }else{
      nsplit <- found[nf]
      temp <- data.frame(
        BASE = substr(casename[i], 1, nsplit - 1),
        VARIANT = substr(casename[i], nsplit + 1, nc),
        stringsAsFactors=FALSE)
      df <- rbind(df, temp)
    }
  }
  return(df)
}

