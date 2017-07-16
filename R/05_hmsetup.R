#==============================================================================
#' @title ReadTemplate:  Read a deck template, and create an hmvars object to store variable characteristics and experimental designs.
#' @description This function reads the variables defined in a template deck, and sets up an object with assumed distribution types and parameters.  It is the constructor for the "hmvars" class.
#' @param template The basename of an Eclipse style template deck.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
#------------------------------------------------------------------------------
ReadTemplate <- function(template = NULL, basedir = "tmp"){
  basedir <- .CheckBasedir(basedir)
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
  # definition and writing out the deck.  The hmvars object needs more thought
  # about its basic design
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
                template_name = objname
  ) # end of class definition
  class(hmvars) <- "hmvars"
  rownames(hmvars$vars) <- NULL
  hmvars$vars$name <- varnames
  hmvars$vars$distribution <- rep("unif",nvars)
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  saveRDS(hmvars, file = rdsfn)
  return(hmvars)
} # end function
#==============================================================================
#' @title EditVar:  Edit parameters in an hmvars object.
#' @description This function allows editing of parameters in the hmvars object as a function of variable name.
#' @param obj The name of an hmvars object.
#' @param pattern A perl compatible regular expression identifying the variable name for which parmeters are being edited.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is used to store the edited object.
#' @param objname This is used to save the edited object to a file. It defaults to the name of the tempate used to create the object
#' @param ... A series of param = value pairs that define how the parameter values associated with a particular variable identified by "pattern" are to be set.
#' @details hmvars$vars dataframe has a row for each variable in the template deck and columns defining the characteristics of the distribution of the variable.  The parameters being edited here are the distribution characteristics for each variable.  The most important characteristics are truncLow and truncHigh, as these values are used to convert back and forth between coded and uncoded experimental designs.  If these values are not supplied, then creating an experimental design will fail.
#'
#' The parameter pairs specified in the ... argument are subject to change, as I am not yet comfortable what this data structure should look like.  Currently, the parameters are: c("distribution", "truncLow", "truncHigh", "param1", "param2", "param3", "discrete1", "discrete2", "discrete3", "discrete4", "discrete5"). The default for distribution is "unif".  The parameters are used as necessary for different distributions, e.g. for "norm", parameter1 is the mean and paramter2 is the standard deviation.  The distribution types are implemented with package "truncdist".
#'
#' The discrete values are for non-numeric choices, e.g. file names of various include files.  These are not currently implemented, but will be soon.  When including discrete values, they should be named carefully.  The names should be legal file names, i.e. alphanumeric characters not starting with a number.  The names will be ordered alpha numerically from lowest to highest, equally spaced from an experimental design point of view. For example, c("p01_relperm", "p50_relperm", "p99_relperm") would have coded design values of c(-1, 0, 1).  Some method of assigning distribution types and probabilities to discrete values may eventually be implemented.
#'
#' @return Returns a hmvars object, and writes out a hmvars object file.
#' @rdname EditVar
#' @export EditVar
#------------------------------------------------------------------------------
EditVar <- function(obj = NULL, pattern = NULL, basedir = "tmp",
                    objname = obj$template_name, ...) {
#   UseMethod("EditVar", obj)
# }
# #==============================================================================
# # #' @return \code{NULL}
# # #' @title EditVar.default:  Edit parameters in an hmvars object.
# # #' @rdname EditVar
# #' @method EditVar default
# # #' @S3method EditVar default
# #' @export
# #------------------------------------------------------------------------------
# EditVar.default <- function(obj=NULL, pattern=NULL, basedir="tmp",
#                             objname = obj$template_name, ...){
#   stop("EditVar only implemented for hmvars.")
# }
# #==============================================================================
# # #' @return \code{NULL}
# # #' @title EditVar.hmvars:  Edit parameters in an hmvars object.
# # #' @rdname EditVar
# #' @method EditVar hmvars
# # #' @S3method EditVar hmvars
# #' @export
# #------------------------------------------------------------------------------
# EditVar.hmvars <- function(obj=NULL, pattern=NULL, basedir="tmp",
#                            objname = obj$template_name, ...){
# an alternate default name:  objname = deparse(substitute(obj))
#  objname <- objname
  if (is.null(obj)) {stop("The object to edited must be supplied")}
  varnames <- obj$vars$name
  if (is.null(pattern)) {
    stop("A pattern to match one or more of the following variables, which ",
         "you wish to edit, must be supplied:  ",
         paste(varnames,collapse = ", "))
  }
  editlines <- grep(pattern,obj$vars$name, perl = TRUE)
  if (!any(editlines)) {
    stop("A pattern to match one or more of the following variables, which ",
         "you wish to edit, must be supplied:  ",
         paste(varnames,collapse = ", "))
  }
  varparams <- names(obj$vars)
  # this gives a list of all of the arguments in the function call, including the ...
  passedparams <- as.list(match.call())[-1]
  ppnames <- names(passedparams)
  # identify which parameters need editing
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
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
   saveRDS(obj, file = rdsfn)
  return(obj)
}
#==============================================================================
#' @title ExpDes:  Create an experimental design using an hmvars object.
#' @description This function reads parameters in the hmvars object as a function of variable name, and uses them in constructing an experimental design.
#' @param obj The name of an hmvars object.
#' @param edtype The type of experimental design.  This should be one of c("pb", "fpb", "augfpb", "lhs").
#' \itemize{
#'   \item \emph{pb} {Plackett-Burman. (FrF2::pb)}
#'   \item \emph{fpb} {Folded Plackett-Burman. (FrF2::fold.design)}
#'   \item \emph{augfpb} {Folded Plackett-Burman augmented with  a latin hypercube spacefilling design. This is the default. (lhs::optAugmentLHS)}
#'   \item \emph{lhs} {A latin hypercube spacefilling design. (lhs::improvedLHS)}
#' }
#' @param ncases The desired number of cases to be run.    This value only constrains the spacefilling portion of the design.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is used to store the edited object.
#' @param ... Arguments passed to the underlying experimental design functions.
#' @param objname This is used to save the edited object to a file. It defaults to the name of the hmvars object.
#' @details   The "ncases" value only constrains the spacefilling portion of the design.  The default of 10 times the number of variables is a common rule of thumb for spacefilling designs.  When "augfpb" is chosen, the number of cases for the "fpb" design is subtracted from the "ncases" value, and this remainder is the number of points that will be augmented with a spacefilling design.
#' @return Returns a template object, and writes out a template object file.
#' @rdname ExpDes
#' @export ExpDes
#------------------------------------------------------------------------------
ExpDes <- function(obj=NULL, edtype = "augfpb", ncases = NULL, basedir = "tmp",
                   objname = obj$template_name, ...){
#   UseMethod("ExpDes", obj)
# }
# #==============================================================================
# # #' @title ExpDes.default:  Create an experimental design using an hmvars object.
# #'
# # #' @rdname ExpDes
# #' @method ExpDes default
# # #' @S3method ExpDes default
# #' @export
# #------------------------------------------------------------------------------
# ExpDes.default <- function(obj=NULL, edtype = "augfpb", ncases = NULL,
#                            basedir = "tmp", objname = obj$template_name, ...){
#   stop("ExpDes only implemented for hmvars.")
#   }
# #==============================================================================
# # #' @title ExpDes.hmvars:  Create an experimental design using an hmvars object.
# #'
# # #' @rdname ExpDes
# #' @method ExpDes hmvars
# # #' @S3method ExpDes hmvars
# #' @export
# #------------------------------------------------------------------------------
# ExpDes.hmvars <- function(obj=NULL, edtype = "augfpb", ncases = NULL,
#                           basedir = "tmp",  objname = obj$template_name, ...){
# objname <- objname
  if (is.null(obj)) {stop("The object to edited must be supplied")}
  edtype.implemented <- c("pb", "fpb", "augfpb", "lhs")
  if (!any(edtype == edtype.implemented)) {
    warning(paste0("Experimental design type ", edtype,
                   " has not yet been implemented."))
    return(obj)
  }
  low.values <- obj$vars$truncLow
  high.values <- obj$vars$truncHigh
  if (!all(high.values >  low.values)) {
    stop(paste0("truncLow and truncHigh values must be supplied for each",
                " variable in the hmvars object, and each truncHigh must",
                " be greater than truncLow"))
  }
  factor.names <- obj$vars$name
  nvars <- length(factor.names)
  if (is.null(ncases)) {
    ncases <- 10 * nvars
  }
  # do pbed and fpbed need to be created here to scope properly?
  # pbed <- data.frame()
  # fpbed <- data.frame()
  # create a pb design
  if (edtype == "pb" | edtype == "fpb" | edtype == "augfpb") {
    nruns <- ceiling((length(factor.names) + 1 ) / 4) * 4
    pbtext <- paste0("FrF2::pb(nruns = nruns, ",
                     "factor.names = factor.names)")
    pbed <- eval(parse(text = pbtext))
  }
  # create fpb design from the previously created pb design
  if (edtype == "fpb" | edtype == "augfpb") {
    foldtext <- "FrF2::fold.design(pbed)"
    # design values are factors, with added descriptive columns
    fpbed <- eval(parse(text = foldtext))
    fpbnames <- colnames(fpbed)
    # this converts the design from factors to values, but changes the names
    fpbed <- DoE.base::desnum(fpbed)
    colnames(fpbed) <- fpbnames
    fpbed <- fpbed[,factor.names]
  }
  # change pb from design class to numeric matrix and add to object obj
  if (edtype == "pb") {
    pbnames <- colnames(pbed)
    # this converts the design from factors to values, but changes the names
    pbed <- DoE.base::desnum(pbed)
    colnames(pbed) <- pbnames
    # remove descriptive columns
    pbed <- pbed[,factor.names]
    obj$expDesignCoded <- pbed
  }
  # add fpb to object obj
  if (edtype == "fpb") {
    obj$expDesignCoded <- fpbed
  }
  if (edtype == "augfpb") {
    fpbcases <- nrow(fpbed)
    newcases <- ncases - fpbcases
    fpb01 <- .Coded2Uncoded(fpbed, 0, 1)
    augfpb01 <- lhs::optAugmentLHS(fpb01, newcases, 5)
    augfpb <- .Uncoded2Coded(augfpb01, 0, 1)
    obj$expDesignCoded <- augfpb
  }
  if (edtype == "lhs") {
    lhs01 <- lhs::improvedLHS(ncases, nvars)
    lhs <- .Uncoded2Coded(lhs01, 0, 1)
    obj$expDesignCoded <- lhs
  }
  rownames(obj$expDesignCoded) <- NULL
  # copy from coded to uncoded for dimensions and column names
  obj$expDesignUncoded <- obj$expDesignCoded
  # calculate the uncoded values
  for (var in obj$vars$name) {
    # this is vectorized, and should work without inner loop
    c <- obj$expDesignCoded[,var]
    lu <- obj$vars$truncLow[obj$vars$name == var]
    hu <- obj$vars$truncHigh[obj$vars$name == var]
    obj$expDesignUncoded[,var] <- .Coded2Uncoded(c, lu, hu)
    # for (i in 1:length(obj$expDesignCoded[,1])) {
    #   c <- obj$expDesignCoded[i,var]
    #   lu <- obj$vars$truncLow[obj$vars$name == var]
    #   hu <- obj$vars$truncHigh[obj$vars$name == var]
    #   obj$expDesignUncoded[i,var] <- .Coded2Uncoded(c, lu, hu)
    # } # end for i
  } # end for var
  rownames(obj$expDesignUncoded) <- NULL
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  saveRDS(obj, file = rdsfn)
  return(obj)
} # end function
#==============================================================================
#' @title AugExpDes:  Add to an existing experimental design in an hmvars object.
#' @description This function reads parameters in the hmvars object as a function of variable name, and uses them in constructing an experimental design.
#' @param obj The name of an hmvars object.
#' @param edtype The type of experimental design.  This should be one of c("aug", "manual").
#' \itemize{
#'   \item \emph{aug} {Existing design augmented with  a latin hypercube spacefilling design. This is the default. (lhs::optAugmentLHS)}
#'   \item \emph{manual} {The existing design is augmented with a matrix generated in some other manner}
#'   }
#' @param ncases The desired number of cases to be added with a spacefilling design.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is used to store the edited object.
#' @param manual.design A manually created design to be added to the hmvars object.  This could concievably be the output from an optimization run on the kriged proxy model so as to update and improve the proxy model.  It could also be an inspired case manually created by the engineer who is doing the history match.
#' @param coded A logical (default is TRUE) indicating if the manual design to augment the hmvars object is coded, or not.
#' @param ... Arguments passed to the underlying experimental design functions.
#' @param objname This is used to save the edited object to a file. It defaults to the name of the hmvars object.
#' @details   The "ncases" value sets the number of cases to be created when augment the lhs design, i.e. when edtype is set to "aug".
#'
#' The experimental designs are coded between -1 and 1 using the truncLow and truncHigh values in the hmvars object.
#' @return Returns a template object, and writes out a template object file.
#' @rdname AugExpDes
#' @export AugExpDes
#------------------------------------------------------------------------------
AugExpDes <- function(obj=NULL, edtype = "aug", ncases = 10, basedir = "tmp",
                      manual.design = NULL, coded = TRUE,
                      objname = obj$template_name, ...){
#   UseMethod("AugExpDes", obj)
# }
# #==============================================================================
# # #' @return \code{NULL}
# #'
# # #' @rdname AugExpDes
# #' @method AugExpDes default
# # #' @S3method AugExpDes default
# #' @export
# #------------------------------------------------------------------------------
# AugExpDes.default <- function(obj=NULL, edtype = "aug", ncases = 10,
#                            basedir = "tmp", manual.design = NULL, coded = TRUE,
#                            objname = obj$template_name, ...){
#   stop("AugExpDes only implemented for hmvars.")
# }
# #==============================================================================
# # #' @return \code{NULL}
# #'
# # #' @rdname AugExpDes
# #' @method AugExpDes hmvars
# # #' @S3method AugExpDes hmvars
# #' @export
# #------------------------------------------------------------------------------
# AugExpDes.hmvars <- function(obj=NULL, edtype = "aug", ncases = 10,
#                           basedir = "tmp",  manual.design = NULL, coded = TRUE,
#                           objname = obj$template_name, ...){
  if (is.null(obj)) {stop("The object to edited must be supplied")}
  edtype.implemented <- c("manual", "aug")
  if (!any(edtype == edtype.implemented)) {
    warning(paste0("Experimental design type ", edtype,
                   " has not yet been implemented for augmentation of an",
                   " existing design."))
    return(obj)
  }
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))

  factor.names <- obj$vars$name
  nvars <- length(factor.names)
  if (edtype == "aug") {
    orig_des <- obj$expDesignCoded
    if (is.null(ncases)) {ncases <- 10}
    if (ncases < 1) {ncases <- 10}
    # convert from (-1, 1) to (0, 1)
    orig_des_01 <- .Coded2Uncoded(orig_des, 0, 1)
    new_des_01 <- lhs::optAugmentLHS(orig_des_01, m = as.integer(ncases),
                                     mult = 5)
    new_des <- .Uncoded2Coded(new_des_01, 0, 1)
    obj$expDesignCoded <- new_des
    # copy from coded to uncoded for dimensions and column names
    obj$expDesignUncoded <- obj$expDesignCoded
    # calculate the uncoded values
    for (var in obj$vars$name) {
      # this is vectorized, and should work without inner loop
      c <- obj$expDesignCoded[,var]
      lu <- obj$vars$truncLow[obj$vars$name == var]
      hu <- obj$vars$truncHigh[obj$vars$name == var]
      obj$expDesignUncoded[,var] <- .Coded2Uncoded(c, lu, hu)
      # for (i in 1:length(obj$expDesignCoded[,1])) {
      #   c <- obj$expDesignCoded[i,var]
      #   lu <- obj$vars$truncLow[obj$vars$name == var]
      #   hu <- obj$vars$truncHigh[obj$vars$name == var]
      #   obj$expDesignUncoded[i,var] <- .Coded2Uncoded(c, lu, hu)
      # } # end for i
    } # end for var
    rownames(obj$expDesignCoded) <- NULL
    rownames(obj$expDesignUncoded) <- NULL
    saveRDS(obj, file = rdsfn)
    return(obj)
  } else if (edtype == "manual") {
    if (!nvars == ncol(manual.design)) {
      stop("The manual design addition doesn't have the same number of",
           " variables as the existing design")
    }
    old_names <- colnames(obj$expDesignCoded)
    new_names <- colnames(obj$expDesignCoded)
    if (is.null(colnames(manual.design))) {
      warning("There are no column names on the new manual design, so you",
              " better have the order right.  Good luck :)")
    } else{
      new_names <- colnames(manual.design)
      if (!setequal(old_names,new_names)) {
        stop("The variable names in the new design are different from those",
             " in the old design.")
      }
    }
    new_design <- manual.design[,old_names]
    if (coded == TRUE) {
      obj$expDesignCoded <- rbind(obj$expDesignCoded, new_design)
      obj$expDesignUncoded <- obj$expDesignCoded
      for (var in obj$vars$name) {
        c <- obj$expDesignCoded[,var]
        lu <- obj$vars$truncLow[obj$vars$name == var]
        hu <- obj$vars$truncHigh[obj$vars$name == var]
        obj$expDesignUncoded[,var] <- .Coded2Uncoded(c, lu, hu)
      } # end for var
    } else {
      obj$expDesignUncoded <- rbind(obj$expDesignUncoded, new_design)
      obj$expDesignCoded <- obj$expDesignUncoded
      for (var in obj$vars$name) {
        u <- obj$expDesignUncoded[,var]
        lu <- obj$vars$truncLow[obj$vars$name == var]
        hu <- obj$vars$truncHigh[obj$vars$name == var]
        obj$expDesignCoded[,var] <- .Uncoded2Coded(c, lu, hu)
      } # end for var
    }
  }
  rownames(obj$expDesignCoded) <- NULL
  rownames(obj$expDesignUncoded) <- NULL
  saveRDS(obj, file = rdsfn)
  return(obj)
}
#==============================================================================
#' @title BuildDecks:  Create a set of decks using a deck template and the experimental design in an hmvars object.
#' @description This function reads an experimental design from a hmvars object and combines it with the appropriate deck template to create a series of decks implementing the experimental design.
#' @param obj The name of an hmvars object.
#' @param template The deck template associated with the hmvars object.
#' @param basedir The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".
#' @param overwrite TRUE means overwrite all; FALSE (the default) means don't overwrite anything; NULL means overwrite decks older than the hmvars object.
#' @param cases The default is NULL, meaning create a deck for all of the cases in the experimental design.  Alternatively, a list of line numbers in the experimental design for the desired cases may be submitted.
#' @details FALSE is the default for overwrite because this would seem to be the most common use case.  One would expect to create a design, build decks, and run some cases.  After looking as the results and thinking a little, one would add some cases to the experimental design and run some more cases.  If the old decks that you have already run were overwritten, they would have a newer file date than the results from the previous runs, and they would be run again.
#' @return Returns a list of deck files suitable for use in submitting the cases
#' @rdname BuildDecks
#' @export BuildDecks
#------------------------------------------------------------------------------
BuildDecks <- function(obj, template, basedir = "tmp", overwrite = FALSE,
                       cases = NULL) {
#   UseMethod("BuildDecks", obj)
# }
# #==============================================================================
# #' @title BuildDecks:  Create a set of decks using a deck template and the experimental design in an hmvars object.
# # #' @return \code{NULL}
# #'
# # #' @rdname BuildDecks
# #' @method BuildDecks default
# # #' @S3method BuildDecks default
# #' @export
# #------------------------------------------------------------------------------
# BuildDecks.default <- function(obj, template, basedir="tmp", overwrite = FALSE, cases = NULL){
#   stop("BuildDecks only implemented for hmvars.")
#   }
# #==============================================================================
# #' @title BuildDecks:  Create a set of decks using a deck template and the experimental design in an hmvars object.
# # #' @return \code{NULL}
# #'
# # #' @rdname BuildDecks
# #' @method BuildDecks hmvars
# # #' @S3method BuildDecks hmvars
# #' @export
# #------------------------------------------------------------------------------
# BuildDecks.hmvars <- function(obj, template, basedir="tmp", overwrite = FALSE, cases = NULL){
  basedir <- .CheckBasedir(basedir)
  decksdir <- file.path(basedir,"DECKS")
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
  width <- max(nchar(as.character(ncases)) + 1, 4)
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
    if (!file.exists(dkname)) {
      writeLines(casedeck, con = dkname)
    } else if (overwrite == TRUE) {
      writeLines(casedeck, con = dkname)
    } else if (is.null(overwrite)) {
      decktime <- file.mtime(dkname)
      objname <- obj$template_name
      rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
      hmvarstime <- file.mtime(rdsfn)
      if (hmvarstime > decktime) {
        writeLines(casedeck, con = dkname)
      }
    }
  }
  return(decklist)
} # end function
#==============================================================================
.Coded2Uncoded <- function(coded, lu, hu, lc = -1, hc = 1){
  # lu = low uncoded; hu = high uncoded
  # lc = low coded; hc = high coded
  if (any(lu >= hu)) {stop(paste("The low uncoded value must be less than the high",
                            "uncoded value"))}
  if (any(lc >= hc)) {stop(paste("The low coded value must be less than the high",
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
  if (any(lu >= hu)) {stop(paste("The low uncoded value must be less than the high",
                            "uncoded value"))}
  if (any(lc >= hc)) {stop(paste("The low coded value must be less than the high",
                            "coded value"))}
  m <- (hc - lc) / (hu - lu)
  b <- lc - m * lu
  coded <- m * uncoded + b
  return(coded)
}
#==============================================================================
.CheckBasedir <- function(basedir){
  if (is.null(basedir)) {basedir <- "tmp"}
  # If the directory structure doesn't exist, this will create it
  ok <- TRUE
  if (!dir.exists(file.path(basedir)) ||
      !dir.exists(file.path(basedir, "OUTPUT")) ||
      !dir.exists(file.path(basedir, "DECKS")) ||
      !dir.exists(file.path(basedir, "REPORTS"))) {
    ok <- suppressWarnings(MakeProj(deckname = NULL, basedir = basedir))
  }
  if (!ok) {stop("Failed to create directory structure")}
  basedir <- normalizePath(basedir)
  return(basedir)
}
#==============================================================================

