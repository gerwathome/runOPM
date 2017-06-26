#' @title Read a deck template, and create a template object.
#' @description This function reads the variables defined in a template deck, and sets up an object with assumed distribution types and parameters.  It is the constructor for the "hmvars" class.
#' @param template The basename of an Eclipse style template deck.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
#------------------------------------------------------------------------------
ReadTemplate <- function(template = NULL, basedir="."){
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  tdp <- character()
  if (!is.null(template)) {
    if (file.exists(template)) {
      tdp <- template
    } else if (file.exists(file.path(decksdir,template))) {
      tdp <- file.path(decksdir,template)
    }else{
      stop(paste("Failed to find template deck", template))
    } # checking for file existance
  } else {
    stop("A template deck must be specified.")
  } # if not null
  objname <- basename(tdp)
  objname <- sub("\\.\\w+$", "", objname, perl = TRUE)
  td <- readLines(tdp)
  varpat <- "^.+{\\$(\\w+)}.+$"
  varnames <- grep(varpat, td, value = TRUE, perl = TRUE)
  varnames <- gsub(varpat, "\\1", varnames, perl = TRUE)
  nvars <- length(varnames)
  # There should be a way to have an undefined number of parameters and
  # discrete values.  Discrete values are expected to be INCLUDE file names.
  #  Also need to figure out how to interpose a function between variable
  # definition and writing out the deck.
  hmvars <- list(vars = data.frame(name = character(nvars),
                                 distribution = character(nvars),
                                 truncLow = numeric(nvars),
                                 truncHigh = numeric(nvars),
                                 param1 = numeric(nvars),
                                 param2 = numeric(nvars),
                                 param3 = numeric(nvars),
                                 discrete1 = character(nvars),
                                 discrete2 = character(nvars),
                                 discrete3 = character(nvars),
                                 discrete4 = character(nvars),
                                 discrete5 = character(nvars),
                                 stringsAsFactors = FALSE
  ),
  expDesignCoded = data.frame(),
  expDesignUncoded = data.frame(),
  casenames = list(),
  template_name = objname
  )
  class(hmvars) <- "hmvars"
  hmvars$vars$name <- varnames
  hmvars$vars$distribution <- rep("unif",nvars)
  if (!dir.exists(decksdir)) {decksdir <- normalizePath(".")}
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  saveRDS(hmvars, file = rdsfn)
  return(hmvars)
} # end function
#==============================================================================
#' @title Edit parameters in a template object.
#' @description This function allows editing of parameters in the hmvars object as a function of variable name.
#' @param obj The name of an hmvars object.
#' @param pattern A perl compatible regular expression identifying the variable name for which parmeters are being edited.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is used to store the edited object.
#' @param ... A series of param=value pairs that define how the parameter values associated with a particular variable identified by "pattern" are to be set.
#' @param objname This is used to save the edited object to a file. It defaults to the name of the tempate used to create the object
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
#------------------------------------------------------------------------------
EditVar <- function(obj=NULL, pattern=NULL, basedir=".",
                    objname = obj$template_name, ...) {
  UseMethod("EditVar", obj)
}

#' @export
EditVar.default <- function(obj=NULL, pattern=NULL, basedir=".",
                            objname = obj$template_name, ...){
  stop("EditVar only implemented for hmvars.")
}

#' @export
#' @rdname EditVar
EditVar.hmvars <- function(obj=NULL, pattern=NULL, basedir=".",
                           objname = obj$template_name, ...){
  # an alternate default name:  objname = deparse(substitute(obj))
  objname <- objname
  if (is.null(obj)) {stop("The object to edited must be supplied")}
  varnames <- obj$vars$name
  editlines <- grep(pattern,obj$vars$name, perl = TRUE)
  if (is.null(pattern) | !any(editlines)) {
    stop("A pattern to match one or more of the following variables, which ",
         "you wish to edit, must be supplied:  ",
         paste(varnames,collapse = ", "))
  }
  varparams <- names(obj$vars)
  passedparams <- as.list(match.call())[-1]
  ppnames <- names(passedparams)
  toedit <- intersect(varparams,ppnames)
  if (length(toedit) < 1) {
    stop("Failed to identify a parameter to edit from the object:  ",
         paste(varparams, collapse = ", "))
  }
  for (i in editlines) {
    for (param in toedit) {
      obj$vars[i,param] <- passedparams[param]
    }
  }
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  if (!dir.exists(decksdir)) {decksdir <- normalizePath(".")}
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
   saveRDS(obj, file = rdsfn)
  return(obj)
}
#==============================================================================
#' @title Create an experimental design using a template object.
#' @description This function reads parameters in the hmvars object as a function of variable name, and uses them in constructing an experimental design.
#' @param obj The name of an hmvars object.
#' @param type The type of experimental design.  Folded Plackett-Burman is the default (and all that is supported, so far.)
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is used to store the edited object.
#' @param ... Arguments passed to the underlying experimental design functions.
#' @param objname This is used to save the edited object to a file. It defaults to the name of the hmvars object.
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
ExpDes <- function(obj=NULL, type = "fpb", basedir=".",
                   objname = obj$template_name, ...){
  UseMethod("ExpDes", obj)
}
#' @export
#------------------------------------------------------------------------------
ExpDes.default <- function(obj=NULL, type = "fpb", basedir=".",
                           objname = obj$template_name, ...){
  stop("ExpDes only implemented for hmvars.")
  }

#' @export
#' @rdname ExpDes
ExpDes.hmvars <- function(obj=NULL, type = "fpb", basedir=".",
                          objname = obj$template_name, ...){
  objname <- objname
  factor.names <- obj$vars$name
  if (type == "fpb") {
    nruns <- ceiling((length(factor.names) + 1 ) / 4) * 4
    pbtext <- paste0("FrF2::pb(nruns = nruns, ",
                     "factor.names = factor.names)")
    pbed <- eval(parse(text = pbtext))
    foldtext <- "FrF2::fold.design(pbed)"
    # design values are factors, with added descriptive columns
    fpbed <- eval(parse(text = foldtext))
    fpbnames <- colnames(fpbed)
    # this converts the design from factors to values, but changes the names
    fpbed <- DoE.base::desnum(fpbed)
    colnames(fpbed) <- fpbnames
    # remove descriptive columns
    fpbed <- fpbed[,factor.names]
    obj$expDesignCoded <- fpbed
  }else{warning("Only folded Plackett-Burman has been implemented so far,")
    return(obj)
  } # end else
  # copy from coded to uncoded for dimensions and column names
  obj$expDesignUncoded <- obj$expDesignCoded
  # calculate the uncoded values
  for (var in obj$vars$name) {
    for (i in 1:length(obj$expDesignCoded[,1])) {
      c <- obj$expDesignCoded[i,var]
      lu <- obj$vars$truncLow[obj$vars$name == var]
      hu <- obj$vars$truncHigh[obj$vars$name == var]
      obj$expDesignUncoded[i,var] <- .Coded2Uncoded(c, lu, hu)
    } # end for i
  } # end for var
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  if (!dir.exists(decksdir)) {decksdir <- normalizePath(".")}
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  saveRDS(obj, file = rdsfn)
  return(obj)
} # end function
#==============================================================================
#' @title Create a set of decks using a deck template and an experimental design.
#' @description This function reads an experimental design from a hmvars object and combines it with the appropriate deck template to create a series of decks implementing the experimental design.
#' @param obj The name of an hmvars object.
#' @param template The deck template associated with the hmvars object.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.
#' @param overwrite TRUE means overwrite all; FALSE means don't overwrite anything; NULL (the defaults) means overwrite decks older than the hmvars object.
#' @param cases The default is NULL, meaning create a deck for all of the cases in the experimental design.  Alternatively, a list of line numbers in the experimental design for the desired cases may be submitted.
#' @details I'm thinking about it.
#' @return Returns a list of deck files suitable for use in submitting the cases
#' @export
#------------------------------------------------------------------------------
BuildDecks <- function(obj, template, basedir=".", overwrite = FALSE, cases = NULL) {
  UseMethod("BuildDecks", obj)
}

#' @export
BuildDecks.default <- function(obj, template, basedir=".", overwrite = FALSE, cases = NULL){
  stop("BuildDecks only implemented for hmvars.")
  }

#' @export
#' @rdname BuildDecks
BuildDecks.hmvars <- function(obj, template, basedir=".", overwrite = FALSE, cases = NULL){
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  if (!dir.exists(decksdir)) {decksdir <- normalizePath(".")}
  if (file.exists(template)) {
    dtp <- template
  }else{
    dtp <- file.path(decksdir,template)
  }
  if (!file.exists(dtp)) {stop(paste0("Deck template file ",
                                    template, " could not be found."))}
  dt <- readLines(con = dtp, warn = FALSE, skipNul = TRUE)
  varpat <- "^.+{\\$(\\w+)}.+$"
  dtnames <- grep(varpat, dt, value = TRUE, perl = TRUE)
  dtnames <- sort(gsub(varpat, "\\1", dtnames, perl = TRUE))
  ed <- obj$expDesignUncoded
  ednames <- sort(colnames(ed))
  if (!identical(dtnames, ednames)) {
    stop(paste0("The variables in the template deck do not agree",
                " with those in the experimental design."))
  }
  ncases <- length(ed[,1])
  width <- nchar(as.character(ncases)) + 1
  padfmt <- paste0("%0", width, "i")
  caselist <- 1:ncases
  if (!is.null(cases)) {
    caselist <- cases
    ncases <- length(cases)
  }
  decklist <- character()
  dkbase <- basename(dtp)
  dkbase <- sub("\\.\\w+$", "", dkbase, perl = TRUE)
  for (i in caselist) {
    dkname <- paste0(dkbase,"_",sprintf(padfmt,i),".DATA")
    dkname <- file.path(decksdir, dkname)
    decklist <- c(decklist, dkname)
    preamble <- dtnames
    postamble <- dt
    for (j in 1:length(dtnames)) {
      name <- dtnames[j]
      val <- ed[i,name]
      preamble[j] <- paste0("-- ", name, " = ", val)
      varpat <- paste0("{\\$", name, "}")
      postamble <- gsub(varpat, val, postamble, perl = TRUE)
    }
    hdr <- paste0("-- Sensitivity variables for case ",i)
    ftr <- paste0("--")
    casedeck <- c(hdr, preamble, ftr, postamble)
    writeLines(casedeck, con = dkname)
  }
  return(decklist)
} # end function
#==============================================================================
.Coded2Uncoded <- function(coded, lu, hu, lc = -1, hc = 1){
  # lu = low uncoded; hu = high uncoded
  # lc = low coded; hc = high coded
  if (lu >= hu) {stop(paste("The low uncoded value must be less than the high",
                            "uncoded value"))}
  if (lc >= hc) {stop(paste("The low coded value must be less than the high",
                            "coded value"))}
  m <- (hu - lu) / (hc - lc)
  b <- lu - m * lc
  uncoded <- m * coded + b
  return(uncoded)
}
#==============================================================================
.Uncoded2Coded <- function(uncoded,lu, hu, lc = -1, hc = 1){
  # lu = low uncoded; hu = high uncoded
  # lc = low coded; hc = high coded
  if (lu >= hu) {stop(paste("The low uncoded value must be less than the high",
                            "uncoded value"))}
  if (lc >= hc) {stop(paste("The low coded value must be less than the high",
                            "coded value"))}
  m <- (hc - lc) / (hu - lu)
  b <- lc - m * lu
  coded <- m * uncoded + b
  return(coded)
}
