#' @title runOPM:  A package to assist in using the Open Porous Media reservoir modeling tools.
#' @description blah blah blah
#'@author George Williams <gerwathome@gmail.com>
#' @docType package
#' @name runOPM
#' @useDynLib runOPM
#' @importFrom Rcpp sourceCpp
NULL

# use "tools::package_native_routine_registration_skeleton(".")" to register c++ 
# to keep CRAN happy with ggplot in the 04_plotting.R file
globalVariables(c("DATE", "VALUE", "CASENAME"))

