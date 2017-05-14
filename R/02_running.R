#' @title Run an Open Porous Media reservoir simulation
#' @description This function deals with the details of running a reservoir simulation model.  One may specify the model deck, the output location, the executable type and version, and the computing resources used to run the job.
#' @param deckname The name and full path to an Eclipse style simulation input deck
#' @param outdir The base directory where the ouput should be written, assumed to be the current directory.
#' @param sim_exec one of ("flow", "flow_mpi", "flow_polymer", "flow_sequential", "flow_solvent")
#' @param restart Is this a restart run? (assumes FALSE)
#' @param sim_version one of c("stable", "latest")
#' @param queue one of c("background", "queue_name", "AWS").  Currently only "background" is functional.
#' @details A subdirectory of the the outdir with the base name of the input deck will be created, keeping each simulation run in its own directory.
#' Restarts probably need to be thought out a little better.
#' The stable executable is assumed to be in /usr/bin and the latest version in /usr/bin/local.  This is consistent with "stable" being the version installed from binaries and "latest" being compiled locally.
#' Only the "background" queue currently works.  Submission to a PBS queue is a short term goal and submission to an AWS batch queue is a longer term goal.
#' It might be desirable to look at the deckname file to determine which executable is appropriate (i.e. which keywords are in the deck).
#' As OPM simulators are not currently supported on Windows, this package is linux specific.
#' @author George Williams
#' @return The function uses system2 to execute the simulation run.  Standard error and standard out are redirected to a file.  A non-zero return indicates an error. The function's purpose is the side effect of submitting a job asynchronously.
#' @references http://opm-project.org/
#'
runflow <- function(deckname,
                     outdir=".",
                     sim_exec="flow",
                     restart=FALSE,
                     sim_version = "stable",
                     queue = "background"){
  exec_path = "/usr/bin/"
  if(sim_version=="latest"){exec_path <- "/usr/local/bin/"}
  # which gives an old version due to my current path
  #    exec <- Sys.which(sim_exec)
  exec <- paste0(exec_path, sim_exec)
  casename <- basename(deckname)
  casename <- sub("[.][^.]*$", "", casename, perl=TRUE)
  output_dir <- file.path(outdir, casename)
  dir.create(output_dir, showWarnings = FALSE)
  if(!restart){
    old <- list.files(output_dir,full.names = TRUE)
    file.remove(old)
    outdeck <- file.path(output_dir, basename(deckname))
    if(!file.exists(outdeck)){
      # this might need to be changed to a copy when submitting job to a queue
      file.symlink(deckname, outdeck)
    }
  }
  args <- c(outdeck, paste0("output_dir=", output_dir))
  sout <- paste0(casename,".OUT")
  sout <- file.path(output_dir, sout)
  serr <- paste0(casename,".OUT")
  serr <- file.path(output_dir, serr)
  system2(exec, args=args, stdout=sout, stderr=serr, wait = FALSE)
}
