#' @title Read a deck template, and create a template object.
#' @description This function reads the variables defined in a template deck, and sets up an object with assumed distribution types and parameters.  It is the constructor for the "hmvars" class.
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
  objname <- basename(tdp)
  objname <- sub("\\.\\w+$","", objname, perl=TRUE)
  td <- readLines(tdp)
  varpat <- "^.+{\\$(\\w+)}.+$"
  varnames <- grep(varpat, td, value=TRUE, perl=TRUE)
  if(ddbg){print(varnames)}
  varnames <- gsub(varpat, "\\1", varnames, perl=TRUE)
  if(ddbg){print(varnames)}
  nvars <- length(varnames)
  # There should probably be a way to have an undefined number of parameters and
  # discrete values.  Discrete values are expected to be INCLUDE file names.
  #  Also need to figure out how to interpose a function between variable
  # definition and writing out the deck.
  hmvars <- list(vars=data.frame(name=character(nvars),
                                 distribution=character(nvars),
                                 truncLow=numeric(nvars),
                                 truncHigh=numeric(nvars),
                                 param1=numeric(nvars),
                                 param2=numeric(nvars),
                                 param3=numeric(nvars),
                                 discrete1=character(nvars),
                                 discrete2=character(nvars),
                                 discrete3=character(nvars),
                                 discrete4=character(nvars),
                                 discrete5=character(nvars),
                                 stringsAsFactors=FALSE
  ),
  expDesignCoded=data.frame(),
  expDesignUncoded=data.frame()
  )
  class(hmvars) <- "hmvars"
  hmvars$vars$name <- varnames
  hmvars$vars$distribution <- rep("unif",nvars)
  if(!dir.exists(decksdir)){decksdir <- normalizePath(".")}
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  if(ddbg){print(paste0("rdsfn_61 = ", rdsfn))}
  saveRDS(hmvars, file = rdsfn)
  return(hmvars)
} # end function

#' @title Edit parameters in a template object.
#' @description This function allows editing of parameters in the hmvars object as a function of variable name.
#' @param obj The name of an hmvars object.
#' @param pattern A perl compatible regular expression identifying the variable name for which parmeters are being edited.
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is used to store the edited object.
#' @param ... A series of param=value pairs that define how the parameter values associated with a particular variable identified by "pattern" are to be set.
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
editvar <- function (obj, ...) {
  UseMethod("editvar", obj)
}

editvar.default <- function(obj, ...){stop("editvar only implemented for hmvars.")}

editvar.hmvars <- function(obj=NULL, pattern=NULL, basedir=".", objname = deparse(substitute(obj)), ... ){
  dbg <- TRUE
  ddbg <- FALSE
  objname <- objname
  if(ddbg){print(paste0("objname_84 = ", objname))}
  if(is.null(obj)){stop("The object to edited must be supplied")}
  varnames <- obj$vars$name
  editlines <- grep(pattern,obj$vars$name, perl=TRUE)
  if(ddbg){
    print("editlines:  ")
    print(paste(editlines, collapse= ", "))
  }
  if(is.null(pattern) | !any(editlines)){
    stop("A pattern to match one or more of the following variables, which ",
         "you wish to edit, must be supplied:  ",
         paste(varnames,collapse=", "))
  }
  varparams <- names(obj$vars)
  passedparams <- as.list(match.call())[-1]
  ppnames <- names(passedparams)
  toedit <- intersect(varparams,ppnames)
  if(length(toedit) < 1){
    stop("Failed to identify a parameter to edit from the object:  ",
         paste(varparams,collapse=", "))
  }
  for(i in editlines){
    for(param in toedit){
      if(ddbg){
        print(paste("param: ", param,
                    "line :", i,
                    ",  from value:  ", passedparams[param],
                    ", to value:  ", obj$vars[i,param]))
      }
      obj$vars[i,param] <- passedparams[param]
    }
  }
  basedir <- normalizePath(basedir)
  decksdir <- file.path(basedir,"DECKS")
  if(!dir.exists(decksdir)){decksdir <- normalizePath(".")}
  rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
  if(ddbg){print(paste0("rdsfn_120 = ", rdsfn))}
  saveRDS(obj, file = rdsfn)
  return(obj)
}
#' @title Create an experimental design using a template object.
#' @description This function reads parameters in the hmvars object as a function of variable name, and uses them in constructing an experimental design.
#' @param obj The name of an hmvars object.
#' @param type The type of experimental design.  Folded Plackett-Burman is the default (and all that is supported, so far.)
#' @param basedir The path to the base directory of a simulation project.  The default is the current directory.  This is used to store the edited object.
#' @details I'm thinking about it.
#' @return Returns a template object, and writes out a template object file.
#' @export
expdes <- function (obj, ...) {
  UseMethod("expdes", obj)
}

expdes.default <- function(obj, ...){stop("expdes only implemented for hmvars.")}

expdes.hmvars <- function(obj=NULL, type = "fpb", basedir=".", objname = deparse(substitute(obj)), ... ){
  dbg <- TRUE
  ddbg <- FALSE
  objname <- objname
  if(ddbg){print(paste0("objname_141 = ", objname))}
  factor.names <- obj$vars$name
  if(type == "fpb"){
    nruns <- ceiling((length(factor.names)+1)/4)*4
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
  for(var in obj$vars$name){
    for(i in 1:length(obj$expDesignCoded[,1])){
      c <- obj$expDesignCoded[i,var]
      lu <- obj$vars$truncLow[obj$vars$name==var]
      hu <- obj$vars$truncHigh[obj$vars$name==var]
      obj$expDesignUncoded[i,var] <- .coded2uncoded(c, lu, hu)
    } # end for i
  } # end for var
basedir <- normalizePath(basedir)
decksdir <- file.path(basedir,"DECKS")
if(!dir.exists(decksdir)){decksdir <- normalizePath(".")}
rdsfn <- file.path(decksdir, paste0(objname, ".rds"))
if(ddbg){print(paste0("rdsfn_175 = ", rdsfn))}
saveRDS(obj, file = rdsfn)
return(obj)
} # end function

.coded2uncoded <- function(coded, lu, hu, lc=-1, hc=1){
  # lu = low uncoded; hu = high uncoded
  # lc = low coded; hc = high coded
  m <- (hu - lu) / (hc - lc)
  b <- lu - m * lc
  uncoded <- m * coded + b
  return(uncoded)
}

.uncoded2coded <- function(uncoded,lu, hu, lc=-1, hc=1){
  # lu = low uncoded; hu = high uncoded
  # lc = low coded; hc = high coded
  m <- (hc - lc) / (hu - lu)
  b <- lc - m * lu
  uncoded <- m * coded + b
  return(coded)
}

# testing during development
dbg <- TRUE
ddbg <- FALSE
spe9vars <- list()
if(dbg){
  basedir <- "/home/gerw/gitrepos/runOPM/inst/extdata/sim"
  template <- "SPE9.TEMPLATE"
  templatepath <- file.path(basedir, "DECKS", template)
  spe9vars <- readtemplate(template = template, basedir = basedir)
  spe9vars <- editvar(spe9vars, pattern = "PORO", truncLow = 0.1,
                      truncHigh = 2, param1 = 0.1, param2 = 2.0,
                      basedir = basedir)
  spe9vars <- editvar(spe9vars, pattern = "PERM", truncLow = 0.1,
                      truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
                      basedir = basedir)
  spe9vars <- expdes(spe9vars, type="fpb",
                     basedir = basedir)
}
if(ddbg){View(spe9vars$vars)}
if(ddbg){View(spe9vars$expDesignCoded)}
if(ddbg){View(spe9vars$expDesignUncoded)}


