#' @title RunFlow:  Run an Open Porous Media reservoir simulation
#' @description This function deals with the details of running a reservoir simulation model.  One may specify the model deck, the output location, the executable type and version, and the computing resources used to run the job.
#' @param decknames A character vector with the name and full path to one or more Eclipse style simulation input decks.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  If a project directory structure does not exist, one will be created.
#' @param sim_exec one of ("flow", "flow_mpi", "flow_polymer", "flow_sequential", "flow_solvent")
#' @param restartcase The base name of the deck that this is a restart from.  The output will be put into the base directory.  The default is NULL.
#' @param sim_version one of c("stable", "latest")
#' @param queue one of c("background", "PBS_queue_name", "AWS").  Currently only "background" is functional.
#' @param overwrite is a logical with a default of NULL. TRUE will always run the case. FALSE will only run the case if the .UNSMRY does not exist.  NULL will run the case if the deck (.DATA) file is newer than the .UNSMRY file.
#' @param wait A logical indicating whether to wait for the model to finish before proceeding.  The default is FALSE, i.e. to run models asynchronously.
#' @details A subdirectory of the the outdir with the base name of the input deck will be created, keeping each simulation run in its own directory.
#'
#' The stable executable is assumed to be in /usr/bin and the latest version in /usr/bin/local.  This is consistent with "stable" being the version installed from binaries and "latest" being compiled locally on a Ubuntu machine.
#'
#' Only the "background" queue currently works.  Submission to a PBS queue is a short term goal and submission to an AWS batch queue is a longer term goal.
#'
#' It might be desirable to look at the deckname file to determine which executable is appropriate (i.e. which keywords are in the deck).
#'
#' This package is linux specific, at this point, becaue I don't havae a Windows or Mac machine to work out the kinks.
#' @author George Williams
#' @return The function uses system2 to execute the simulation run.  Standard error and standard out are redirected to a file.  A non-zero return indicates an error. The function's purpose is the side effect of submitting a job to run somewhere.
#' @references \href{http://opm-project.org/}{The Open Porous Media initiative}
#' @export
#'
RunFlow <- function(decknames,
                    basedir="tmp",
                    sim_exec="flow",
                    restartcase=NULL,
                    sim_version = "stable",
                    queue = "background",
                    overwrite = NULL,
                    wait= FALSE){
  for (deckname in decknames) {
    if (!file.exists(deckname)) {
      deckname <- file.path(basedir,"DECKS",deckname)
      if (!file.exists(deckname)) {
        stop(paste0("Failed to find simulation deck ", deckname))
      }
    }
    # If the directory structure doesn't exist, this will create it and copy in
    # the deck
    ok <- TRUE
    if (!dir.exists(file.path(basedir, "OUTPUT")) ||
       !dir.exists(file.path(basedir, "DECKS")) ||
       !dir.exists(file.path(basedir, "REPORTS"))) {
      ok <- MakeProj(deckname, basedir)
      if (!ok) {stop("Failed to create directory structure")}
    }
    casename <- basename(deckname)
    fromdeck <- normalizePath(deckname) # this must exist
    deck <- file.path(basedir, "DECKS", casename)
    todeck <- suppressWarnings(normalizePath(deck)) # this may exist
    # to avoid the possibility of copying a file on top of itself
    if (!identical(fromdeck, todeck)) {file.copy(fromdeck, todeck,
                                                 overwrite = TRUE)}
    # This creates the output directory for this run
    casename <- sub("[.][^.]*$", "", casename, perl = TRUE)
    output_dir <- file.path(basedir, "OUTPUT", casename)
    if (!is.null(restartcase)) {output_dir <- file.path(basedir, "OUTPUT",
                                                      restartcase)}
    if (!dir.exists(output_dir)) {
      ok <- dir.create(output_dir, showWarnings = FALSE)
      if (!ok) {stop("Failed to create output directory.")}
    }
    exec_path = "/usr/bin/"
    if (sim_version == "latest") {exec_path <- "/usr/local/bin/"}
    exec <- paste0(exec_path, sim_exec)
    if (!file.exists(exec)) {stop("Failed to locate the desired executable.")}

    # get the timestamp of the deck and unsmry files for overwrite testing
    deckts <- file.mtime(todeck)
    # tosmry should only have one file, but it is possible multiple runs may
    # have different file formats; pick the newest
    tosmry <- suppressWarnings(.FindSummary(basedir = output_dir,
                                            casename = casename,
                                            recursive = FALSE))
    if (length(tosmry > 1)) {
      tsold <- 0
      fnold <- ""
      for (fn in tosmry) {
        ts <- file.mtime(fn)
        if (ts > tsold) {
          tsold <- ts
          fnold <- fn
        } # end if
      } # end for
      tosmry <- fnold
    } # end tosmry > 1
    smryts <- 0
    if (length(tosmry) > 0) {
      smryts <- file.mtime(tosmry)
    }
    casepattern <- paste0(casename,"\\..+")
    old <- list.files(path = output_dir,
                      pattern = casepattern,
                      full.names = TRUE)
    if (is.null(overwrite)) {
      if (deckts > smryts) {
        file.remove(old)
      }else{
        return(warning("Case not run because existing summary is newer than input deck."))
      }

    }else if (overwrite) {
      file.remove(old)
    }else if (length(tosmry) > 0) {
      return(warning("Case not run to avoid overwriting previous results."))
    }
    args <- c(deck, paste0("output_dir=", output_dir))
    sout <- paste0(casename,".OUT")
    sout <- file.path(output_dir, sout)
    serr <- paste0(casename,".OUT")
    serr <- file.path(output_dir, serr)
    system2(exec, args = args, stdout = sout, stderr = serr, wait = wait)
  } # end for decknames
} # end function
