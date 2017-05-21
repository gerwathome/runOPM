#' @title Create a project directory to run simulation cases
#' @description This function sets up a standard directory structure for running one or more simulation cases.
#' @param deckname The full path to, and name of, a simulation deck.  The default is a deck from the first SPE comparative solution project.  This could also be a deck template that will later be used to generate multiple case decks.
#' @param basedir is the base directory of the simulation project.  The default is the current directory.  In general, it is not a good idea to use the current directory, as it may lead to confusion or overwriting of files.  If a string is supplied to the basedir argument, a subdirectory of the current directory will be created as the project directory.
#' @details If the runflow function is called without an appropriate directory structure, this function is called to create one.  This function may also be called directly by the user, to create a directory structure with a deck template, rather than a runnable deck.
#'
#' The directory structure includes three subdirectories of the basedir:  a DECKS directory where decks and deck templates are stored, an OUTPUT directory for simulation run output and a REPORTS directory for storing information concerning multiple runs.  When a model is run, the output will be placed in a subdirectory of the OUTPUT directory with the same name as the deck.
#' @return Returns FALSE if any of the creation or copying tasks failed.
#' @export
makeproj <- function(deckname = NULL, basedir = "."){
  if(is.null(deckname)){
    deckname <- system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
  }
  if(!dir.exists(basedir)){
    dir.create(basedir, showWarnings = FALSE)
  }
  ok1 <- dir.create(file.path(basedir, "DECKS"), showWarnings = FALSE)
  ok2 <- file.copy(deckname, file.path(basedir, "DECKS"))
  ok3 <- dir.create(file.path(basedir, "OUTPUT"), showWarnings = FALSE)
  ok4 <- dir.create(file.path(basedir, "REPORTS"), showWarnings = FALSE)
  ok <- all(c(ok1, ok2, ok3, ok4))
  return(ok)
}
