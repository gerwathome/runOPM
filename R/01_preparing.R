#' @title Create a project directory to run simulation cases
#' @description This function sets up a standard directory structure for running one or more simulation cases.
#' @param deckname The full path to, and name of, a simulation deck.  The default is no deck.  This could also be a deck dataveclate that will later be used to generate multiple case decks.
#' @param basedir is the base directory of the simulation project.  The default is the current directory.  In general, it is not a good idea to use the current directory, as it may lead to confusion or overwriting of files.  If a string is supplied to the basedir argument, a subdirectory of the current directory will be created as the project directory.
#' @details If the runflow function is called without an appropriate directory structure, this function is called to create one.  This function may also be called directly by the user, to create a directory structure with a deck dataveclate, rather than a runnable deck.
#'
#' The directory structure includes three subdirectories of the basedir:  a DECKS directory where decks and deck dataveclates are stored, an OUTPUT directory for simulation run output and a REPORTS directory for storing information concerning multiple runs.  When a model is run, the output will be placed in a subdirectory of the OUTPUT directory with the same name as the deck.
#' @return Returns FALSE if any of the creation or copying tasks failed.
#' @export
MakeProj <- function(deckname = NULL, basedir = "."){
  ok1 <- ok2 <- ok3 <- ok4 <- ok5 <- TRUE
  if (!dir.exists(basedir)) {
    ok1 <- dir.create(basedir)
  }
  if (!dir.exists(file.path(basedir, "DECKS"))) {
    ok2 <- dir.create(file.path(basedir, "DECKS"))
  }
  if (!is.null(deckname)) {
    if (file.exists(deckname)) {
      ok3 <- file.copy(deckname, file.path(basedir, "DECKS"))
    } else {
      warning(paste0("Failed to find deck", deckname))
    }
  }
  if (!dir.exists(file.path(basedir, "OUTPUT"))) {
    ok4 <- dir.create(file.path(basedir, "OUTPUT"))
  }
  if (!dir.exists(file.path(basedir, "REPORTS"))) {
    ok5 <- dir.create(file.path(basedir, "REPORTS"))
  }
  ok <- all(c(ok1, ok2, ok3, ok4, ok5))
  return(ok)
}

#' @title Read in in data deck
#' @description This function reads selected data from a data deck into R
#' @param deckname The full path to, and name of, a simulation deck.  There is no default, and the function will fail without an input deck.
#' @param props is a list of the names of the properties that one wishes to retrieve.
#' @details The current version of this function is very simple, and will read only a very limited selection of grid data.  It does not understand windows of data or multiple occurances of a keyword, and return values are unpredictable in these cases.  It will follow up INCLUDE files and expand multiple values expressed with an intermediate "*".  It expects the number of cell values found to be equal to the product of the DIMENS values.
#' @return Returns a data frame with the desired data.
#' @export
ReadDeck <- function(deckname = NULL, props = c("PORO")){
  if (!file.exists(deckname)) {stop(paste("Failed to find deck", deckname))}
  # the point of this function is to bring the include files into the deck
  deckdatalines <- .GetDeckLines(deckname)
  griddims <- as.numeric(.GetDataVec("DIMENS", deckdatalines))
  cellcount <- prod(griddims)
  propcount <- length(props)
  df <- data.frame(matrix(ncol = propcount, nrow = cellcount))
  colnames(df) <- props
  for (prop in props) {
    datavec <- .GetDataVec(prop, deckdatalines)
    if (cellcount != length(datavec)) {
      warning(paste0("Cellcount is ", cellcount, ", but found only ",
                     length(datavec), "cell values for ", prop))}
    df[, prop] <- datavec
  } # for prop
  I <- 1:griddims[1]
  J <- 1:griddims[2]
  K <- 1:griddims[3]
  ijk <- expand.grid(I,J,K)
  colnames(ijk) <- c("I", "J", "K")
  df <- cbind(ijk, df)
  return(df)
} # end func

.ExpandVals <- function(multdata){
  pat <- "(\\d+\\.*\\d*)\\*(\\d+\\.*\\d*)"
  count <- as.numeric(sub(pat, "\\1", multdata, perl = TRUE))
  val <- as.numeric(sub(pat, "\\2", multdata, perl = TRUE))
  cellvals <- try(rep(val, count))
  return(cellvals)
}

.GetDataVec <- function(prop, deckdatalines){
  # this function assumes the include files have already been brought in
  dump <- grepl("^--.+|^\\s*$", deckdatalines, perl = TRUE)
  deckdatalines <- deckdatalines[!dump]
  slashlines <- grep("\\/",deckdatalines, perl = TRUE)
  proppattern <- paste0("^", prop, "\\s*$")
  proplines <- grep(proppattern, deckdatalines, perl = TRUE)
  if (length(proplines) > 1) {
    print(paste0("Lines found with ", prop, ":"))
    print(proplines)
    warning(paste0("This function isn't smart enough to",
                   " deal with mulitple entries for the",
                   " same property, yet"))
  }
  # Need to add support for ADD, BOX, COPY, ENDBOX, EQUALS, MAXVALUE, MINVALUE,
  # and MULTIPLY.  I don't know how many of these flow supports.
  begline <- proplines[1] + 1
  # find the first slash after the property keyword
  endline <- slashlines[begline <= slashlines][1]
  datalines <- deckdatalines[begline:endline]
  datalines <- gsub("^\\s*", "", datalines, perl = TRUE)
  datalines <- gsub("\\s*$", "", datalines, perl = TRUE)
  datalines <- gsub("\\s*\\/$", "", datalines, perl = TRUE)
  datalines <- gsub("\\t", "", datalines, perl = TRUE)
  datavec <- unlist(strsplit(datalines, "\\s+"))
  cellvals <- NULL
  for (i in 1:length(datavec)) {
    if (any(grep("\\*", datavec[i], perl = TRUE))) {
      cellvals <- c(cellvals, .ExpandVals(datavec[i]))
    }else{
      cellvals <- c(cellvals, as.numeric(datavec[i]))
    } # end if else
  } # end for each in datavec

  datavec <- as.vector(cellvals)
  return(datavec)
}

.GetDeckLines <- function(deckname){
  # the point of this function is to bring the include files into the deck
  deckdatalines <- readLines(deckname, warn = FALSE)
  dirpath <- dirname(deckname)
  includelines <- grep("INCLUDE", deckdatalines, perl = TRUE)
  if (!any(includelines)) {return(deckdatalines)}
  slashlines <- grep("\\/\\s*$", deckdatalines, perl = TRUE)
  linesout <- vector()
  slashline <- NULL
  for (i in 1:length(includelines)) {
    includeline <- includelines[i]
    # the first line for this deck chunk starts right after the last slash
    begline <- ifelse(is.null(slashline), 1, slashline + 1)
    # find the first trailing slash after the current INCLUDE
    slashline <- slashlines[includeline < slashlines][1]
    # the end line for this deck chunk is just before the INCLUDE
    endline <- includeline - 1
    linesout <- c(linesout, deckdatalines[begline:endline])
    # next add in the lines from the INCLUDE file
    # the Eclipse manual states the file name  should be on the next line
    fn <- .GetFN(deckdatalines[includeline + 1])
    fn <- file.path(dirpath, fn)
    includechunk <- readLines(fn, warn = FALSE)
    linesout <- c(linesout, includechunk)
  }
  # finally, add in the original deck remains after the last INCLUDE file
  begline <- slashline + 1
  endline <- length(deckdatalines)
  linesout <- c(linesout, deckdatalines[begline:endline])
  return(linesout)
}

.GetFN <- function(deckline){
# clean up all of the crap from the line giving the INCLUDE file name
  fn <- gsub("^\\s*", "", deckline)
  fn <- gsub("\\s*$", "", fn)
  fn <- gsub("\\s*\\/$", "", fn)
  fn <- gsub("\\t", "", fn)
  fn <- gsub("'", "", fn)
  fn <- gsub("\"", "", fn)
  return(fn)
}
