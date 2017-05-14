#' @title Plot simulation results from csv files
#' @description This function accepts multiple csv files as input, and plots history match plots using ggplot
#' @param basefile The full path (relative or absolute) to a csv file with data to be used to compare against multiple cases, e.g the history file to be compared with multiple model runs.
#' @param compfiles A list of csv files with data to be compared against the base file.
#' @param template A file describing which data is to be used in the comparison plots
#' @param outdir Where the output plots should be stored.
#plotflow <- function(basefile, compfiles, template, outdir="."){
prep <- function(casedata){
    1
}
