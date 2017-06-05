#' @title Read a deck template, and create a template object.
#' @description This function reads the variables defined in a template deck, and sets up an object with assumed distribution types and parameters.
#' @param template The basename of an Eclipse style template deck.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
#------------------------------------------------------------------------------
readtemplate <- function(template = NULL, basedir="."){
  dbg <- TRUE
  ddbg <- FALSE
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  tdp <- character()
  if(!is.null(template)){
    if(file.exists(template)){
      tdp <- template
    }else if(file.exists(file.path(decksdir,template))){
      tdp <- file.path(decksdir,template)
    }else{
      stop(paste("Failed to find template deck", template))
    } # checking for file existance
}else{
  stop("A template deck must be specified.")
} # if not null
  td <- readLines(tdp)
  varpat <- "{\\$(\\w+)}"
  vars <- grep(varpat, td, value=TRUE, perl=TRUE)
  if(dbg){vars}
  vars <- gsub(varpat, "\\1", vars, perl=TRUE)
  if(dbg){vars}
  to <- "I need to learn how to define an object"
  return(to)
} # end function
