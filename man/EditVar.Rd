% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_hmsetup.R
\name{EditVar}
\alias{EditVar}
\title{EditVar:  Edit parameters in an hmvars object.}
\usage{
EditVar(obj = NULL, pattern = NULL, basedir = "tmp",
  objname = obj$template_name, ...)
}
\arguments{
\item{obj}{The name of an hmvars object.}

\item{pattern}{A perl compatible regular expression identifying the variable name for which parmeters are being edited.}

\item{basedir}{The path to the base directory of a simulation project.  The default is a subdirectory of the current directory called "tmp".  This is used to store the edited object.}

\item{objname}{This is used to save the edited object to a file. It defaults to the name of the template used to create the object}

\item{...}{A series of param = value pairs that define how the parameter values associated with a particular variable identified by "pattern" are to be set.}
}
\value{
Returns a hmvars object, and writes out a hmvars object file.
}
\description{
This function allows editing of parameters in the hmvars object as a function of variable name.
}
\details{
hmvars$vars dataframe has a row for each variable in the template deck and columns defining the characteristics of the distribution of the variable.  The parameters being edited here are the distribution characteristics for each variable.  The most important characteristics are truncLow and truncHigh, as these values are used to convert back and forth between coded and uncoded experimental designs.  If these values are not supplied, then creating an experimental design will fail.

The parameter pairs specified in the ... argument are subject to change, as I am not yet comfortable what this data structure should look like.  Currently, the parameters are: c("distType", "truncLow", "truncHigh", "param1", "param2", "param3", "discrete1", "discrete2", "discrete3", "discrete4", "discrete5"). The default for distType is "unif".  The parameters are used as necessary for different distributions, e.g. for "norm", parameter1 is the mean and paramter2 is the standard deviation.  The distribution types are implemented with package "truncdist".

The discrete values are for non-numeric choices, e.g. file names of various include files.  These are not currently implemented, but will be soon.  When including discrete values, they should be named carefully.  The names should be legal file names, i.e. alphanumeric characters not starting with a number.  The names will be ordered alpha numerically from lowest to highest, equally spaced from an experimental design point of view. For example, c("p01_relperm", "p50_relperm", "p99_relperm") would have coded design values of c(-1, 0, 1).  Some method of assigning distribution types and probabilities to discrete values may eventually be implemented.
}
