% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_hmsetup.R
\name{AugExpDes}
\alias{AugExpDes}
\title{AugExpDes:  Add to an existing experimental design in an hmvars object.}
\usage{
AugExpDes(obj = NULL, edtype = "aug", ncases = 10, basedir = "tmp",
  manual.design = NULL, coded = TRUE, objname = obj$template_name, ...)
}
\arguments{
\item{obj}{The name of an hmvars object.}

\item{edtype}{The type of experimental design.  This should be one of c("aug", "manual").
\itemize{
  \item \emph{aug} {Existing design augmented with  a latin hypercube spacefilling design. This is the default. (lhs::optAugmentLHS)}
  \item \emph{manual} {The existing design is augmented with a matrix generated in some other manner}
  }}

\item{ncases}{The desired number of cases to be added with a spacefilling design.}

\item{basedir}{The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is used to store the edited object.}

\item{manual.design}{A manually created design to be added to the hmvars object.  This could concievably be the output from an optimization run on the kriged proxy model so as to update and improve the proxy model.  It could also be an inspired case manually created by the engineer who is doing the history match.}

\item{coded}{A logical (default is TRUE) indicating if the manual design to augment the hmvars object is coded, or not.}

\item{objname}{This is used to save the edited object to a file. It defaults to the name of the hmvars object.}

\item{...}{Arguments passed to the underlying experimental design functions.}
}
\value{
Returns a template object, and writes out a template object file.
}
\description{
This function reads parameters in the hmvars object as a function of variable name, and uses them in constructing an experimental design.
}
\details{
The "ncases" value sets the number of cases to be created when augment the lhs design, i.e. when edtype is set to "aug".

The experimental designs are coded between -1 and 1 using the truncLow and truncHigh values in the hmvars object.
}
