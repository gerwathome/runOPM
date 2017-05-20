#' @title Create a project directory to run simulation cases
#' @description This function sets up a standard directory structure for running one or more simulation cases.  If the runflow function is called without an appropriate directory structure, this function is called to create one.  It may also be called by the user, if the project is being started with a deck template, rather than a runnable deck.
#' @param deckname The full path to, and name of, a simulation deck.  The default is a deck from the first SPE comparative solution project.  This could also be a deck template that will later be used to generate multiple case decks.
#' @param basedir is the base directory of the simulation project.  The default is the current directory.  In general, it is not a good idea to use the current directory, as it may lead to confusion or overwriting of files.  If a string is supplied to the basedir argument, a subdirectory of the current directory will be created as the project directory.
#' @export
makeproj <- function(deckname = NULL, basedir = "."){
  if(is.null(deckname)){
    deckname <- system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
  }
  if(!dir.exists(basedir)){
    dir.create(basedir, showWarnings = FALSE)
  }
  dir.create(file.path(basedir, "DECKS"), showWarnings = FALSE)
  file.copy(deckname, file.path(basedir, "DECKS"))
  dir.create(file.path(basedir, "OUTPUT"), showWarnings = FALSE)
  dir.create(file.path(basedir, "REPORTS"), showWarnings = FALSE)
}
