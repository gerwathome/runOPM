#' @title Run an Open Porous Media reservoir simulation
#' @description This function deals with the details of running a reservoir simulation model.  One may specify the model deck, the output location, the executable type and version, and the computing resources used to run the job.
#' @param deckname The name and full path to an Eclipse style simulation input deck
#' @param basedir The base directory of a simulation project, assumed to be the current directory.  If a project directory structure does not exist, one will be created.
#' @param sim_exec one of ("flow", "flow_mpi", "flow_polymer", "flow_sequential", "flow_solvent")
#' @param restart Is this a restart run? (assumes FALSE)
#' @param sim_version one of c("stable", "latest")
#' @param queue one of c("background", "PBS_queue_name", "AWS").  Currently only "background" is functional.
#' @param wait A logical indicating whether to wait for the model to finish before proceeding.  The default is FALSE, i.e. to run models asynchronously.
#' @details A subdirectory of the the outdir with the base name of the input deck will be created, keeping each simulation run in its own directory.
#'
#' Restarts probably need to be thought out a little better, and may not work at all, yet.
#'
#' The stable executable is assumed to be in /usr/bin and the latest version in /usr/bin/local.  This is consistent with "stable" being the version installed from binaries and "latest" being compiled locally on a Ubuntu machine.
#'
#' Only the "background" queue currently works.  Submission to a PBS queue is a short term goal and submission to an AWS batch queue is a longer term goal.
#'
#' It might be desirable to look at the deckname file to determine which executable is appropriate (i.e. which keywords are in the deck).
#'
#' As OPM simulators are not currently supported on Windows, this package is linux specific.
#' @author George Williams
#' @return The function uses system2 to execute the simulation run.  Standard error and standard out are redirected to a file.  A non-zero return indicates an error. The function's purpose is the side effect of submitting a job to run somewhere.
#' @references http://opm-project.org/
#' @export
#'
runflow <- function(deckname,
                    basedir=".",
                    sim_exec="flow",
                    restart=FALSE,
                    sim_version = "stable",
                    queue = "background",
                    wait= FALSE){
  if(!file.exists(deckname)){
    stop("Please provide a simulation deck to run.")
  }
  # If the directory structure doesn't exist, this will create it and copy in
  # the deck
  ok <- TRUE
  if(!dir.exists(file.path(basedir, "OUTPUT")) ||
     !dir.exists(file.path(basedir, "DECKS")) ||
     !dir.exists(file.path(basedir, "REPORTS"))){
    ok <- makeproj(deckname, basedir)
    if(!ok){stop("Failed to create directory structure")}
  }
  # If the directory exists, but the deck isn't there, this will copy it
  casename <- basename(deckname)
  deck <- file.path(basedir, "DECKS", casename)
  if(!file.exists(deck)){file.copy(deckname, deck)}
  # This creates the output directory for this run
  casename <- sub("[.][^.]*$", "", casename, perl=TRUE)
  output_dir <- file.path(basedir, "OUTPUT", casename)
  if(!dir.exists(output_dir)){
    ok <- dir.create(output_dir, showWarnings = FALSE)
    if(!ok){stop("Failed to create output directory.")}
  }
  exec_path = "/usr/bin/"
  if(sim_version=="latest"){exec_path <- "/usr/local/bin/"}
  exec <- paste0(exec_path, sim_exec)
  if(!file.exists(exec)){stop("Failed to locate the desired executable.")}

  if(!restart){
    old <- list.files(output_dir,full.names = TRUE)
    file.remove(old)
  }
  args <- c(deck, paste0("output_dir=", output_dir))
  sout <- paste0(casename,".OUT")
  sout <- file.path(output_dir, sout)
  serr <- paste0(casename,".OUT")
  serr <- file.path(output_dir, serr)
  system2(exec, args=args, stdout=sout, stderr=serr, wait = wait)
}
