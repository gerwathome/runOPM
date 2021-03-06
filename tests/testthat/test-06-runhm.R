context("Running the HM, gathering results and calculating errors")

#==============================================================================
# setwd("/home/gerw/gitrepos/runOPM/tests/testthat")
basedir <- "spe9hm"
basedir <- .CheckBasedir(basedir)
hist_csv_name <-  system.file("testdata", "HIST_CSV.csv", package = "runOPM")
hist_xlsx_name <-  system.file("testdata", "HIST_XLSX.xlsx",
                               package = "runOPM")
long_csv <- ImportHist(ssheetfile = hist_csv_name, basedir = basedir)
long_xlsx <- ImportHist(ssheetfile = hist_xlsx_name, basedir = basedir)
csv_df <- long_xlsx[long_xlsx$CASENAME == "HIST_CSV", c(2:7, 9:10)]
xlsx_df <- long_xlsx[long_xlsx$CASENAME == "HIST_XLSX", c(2:7, 9:10)]
# all.equal(csv_df, xlsx_df)
#------------------------------------------------------------------------------
test_that("ImportHist works", {
  # check that each method brings in the same data
  expect_equivalent(csv_df, xlsx_df)
})
#==============================================================================
# setwd("/home/gerw/gitrepos/runOPM/tests/testthat/")
unlink("spe9hm", recursive = TRUE)
template <- "SPE9.TEMPLATE"
from.template.path <-  system.file("extdata", template, package = "runOPM")
from.perm.inc.path <-  system.file("extdata", "GRID", "PERMVALUES.INC",
                                   package = "runOPM")
from.grdecl.path <-  system.file("extdata", "GRID", "SPE9.GRDECL",
                                 package = "runOPM")
basedir <- "spe9hm"
MakeProj(deckname = from.template.path, basedir = basedir)
basedir <- file.path(getwd(), basedir)
deckdir <- file.path(basedir, "DECKS")
templatepath <- file.path(deckdir, template)
griddir <- file.path(deckdir, "GRID")
if (!dir.exists(griddir)) {dir.create(griddir)}
file.copy(from.perm.inc.path, file.path(griddir, "PERMVALUES.INC"),
          overwrite = TRUE)
file.copy(from.grdecl.path, file.path(griddir, "SPE9.GRDECL"),
          overwrite = TRUE)
spe9vars <- ReadTemplate(template, "spe9hm")
# 30 variables
# spe9vars <- EditVar(spe9vars, pattern = "PORO", truncLow = 0.1,
#                      truncHigh = 2, param1 = 0.1, param2 = 2.0,
#                      basedir = basedir)
# spe9vars <- EditVar(spe9vars, pattern = "PERM", truncLow = 0.1,
#                      truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
#                      basedir = basedir)
# 3 variables
spe9vars <- EditVar(spe9vars, pattern = "PORO", truncLow = 1,
                    truncHigh = 1, param1 = 1, param2 = 1,
                    basedir = basedir)
spe9vars <- EditVar(spe9vars, pattern = "PERM", truncLow = 1,
                    truncHigh = 1, param1 = 1, param2 = 1,
                    basedir = basedir)
spe9vars <- EditVar(spe9vars, pattern = "PERMX_0[123]", truncLow = 0.1,
                    truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
                    basedir = basedir)

 set.seed(424242)
 spe9vars <- ExpDes(spe9vars, edtype = "augfpb", basedir = basedir)
 spe9decks <- BuildDecks(spe9vars, template = templatepath,  basedir = basedir,
                         overwrite = NULL, cases = NULL)
 # add hist data to PROJSUM.csv file
 hist_csv_name <-  system.file("testdata", "HIST_CSV.csv", package = "runOPM")
 long_csv <- ImportHist(ssheetfile = hist_csv_name, basedir = basedir)
# this runs the augment 30 case deck list
ok <- RunFlow(spe9decks, basedir = basedir, overwrite = NULL, wait = TRUE)
if (!is.null(ok)) {print(paste0("value of RunFlow ok is ", ok))}
spe9rslts <- EclSum(basedir = basedir)

# projsumfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
# spe9rslts <- readr::read_csv(projsumfn, col_types = .LongColSpec())
#PlotEach(spe9rslts, wgnames = "FIELD")
long <- CalcErrors(long = spe9rslts, basecase = "HIST_CSV",
                               basedir = basedir)

# psfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
# long <- readr::read_csv(psfn, col_types = .LongColSpec())
# element_error <- ErrorByElement(long = long, basedir = basedir)
# eefn <- file.path(basedir, "REPORTS", "ElementError.csv")
# eesum <- readr::read_csv(eefn, col_types = .ErrorByElementColSpec())
# head(element_error)
# summary(element_error)
member_error <- ErrorByMember(long = long, basedir = basedir)
spe9_mem_err <- member_error
# mefn <- file.path(basedir, "REPORTS", "MemberErrorLong.csv")
# member_error <- readr::read_csv(mefn, col_types = .ErrorByElemLongColSpec())
# head(member_error)
# summary(member_error)

#------------------------------------------------------------------------------
test_that("Error calculation and summarizing works", {
  expect_equal_to_reference(spe9vars$expDesignCoded,
                            "spe9vars_expdes_coded_30.rds")
  expect_equal_to_reference(long, "test_calc_errors.rds")
  # expect_equal_to_reference(element_error, "test_element_errors.rds")
  expect_equal_to_reference(member_error, "test_member_errors.rds")
})
#==============================================================================
spe9_mod_sel <- SelectModels(member_error = spe9_mem_err,
                             basedir = "spe9hm",
                             wgnames = c("FIELD"),
                             keywords = c("WOPR", "WGPR", "WWPR"),
                             errortypes = c("MEAN_FRAC_ERR",
                                            "ABS_MEAN_FRAC_ERR"))
# spe9_mod_sel$all_choices
# spe9_mod_sel$WGNAME
# spe9_mod_sel$KEYWORD
# spe9_mod_sel$ERRORTYPE
# spe9_mod_sel$filt
# spe9_mod_sel$choice
# spe9_mod_sel$kmfilt

spe9.km <- BuildKModels(hmvars = spe9vars, member_error = spe9_mem_err,
                       model_selection = spe9_mod_sel, basedir = "spe9hm")
#------------------------------------------------------------------------------
test_that("SelectModels and BuildKModels works", {
  expect_equal(1, 1)
})

#==============================================================================
# rm(list=ls())
# basedir <- "/home/gerw/gitrepos/runOPM/tests/testthat/spe9hm"
# hmvars_path <- file.path(basedir, "DECKS", "SPE9.rds")
# spe9vars <- readRDS(hmvars_path)
# objname <- spe9vars$template_name
# fn <- file.path(basedir, "REPORTS", "MemberErrorLong.csv")
# spe9_mem_err <- readr::read_csv(fn)
# projsumfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
# spe9rslts <- readr::read_csv(projsumfn, col_types = runOPM:::.LongColSpec())
# report_path <- file.path(basedir, "REPORTS")
# ms_path <- file.path(report_path, "MemberSelection.rds")
# spe9_mod_sel <- readRDS(ms_path)
spe9_varsens <- ModelSensitivity(hmvars = spe9vars, member_error = spe9_mem_err,
                                 model_selection = spe9_mod_sel,
                                 basedir = basedir)
#------------------------------------------------------------------------------
test_that("ModelSensitivity works", {
  expect_equal(1, 1)
})
#==============================================================================
# km_path <- file.path(report_path, paste0(objname, "_km.rds"))
# spe9.km <- readRDS(km_path)
# spe9.km3 <- spe9.km[1:3]
# spe9_opt3 <- RunGPareto(kmodels = spe9.km3, method = "genoud",
#                         basedir = basedir)
#
spe9_opt <- RunGPareto(kmodels = spe9.km, method = "genoud", basedir = basedir)
#------------------------------------------------------------------------------
test_that("RunGPareto works", {
  expect_equal(1, 1)
})

#==============================================================================
# sens_path <- file.path(report_path, paste0(objname, "_varsens.rds"))
# spe9_varsens <- readRDS(sens_path)
# opt_path <- file.path(report_path, paste0(objname, "_km_opt.rds"))
# spe9_opt <- readRDS(opt_path)
#------------------------------------------------------------------------------
test_that("More stuff works", {
  expect_equal(1, 1)
})

#==============================================================================
# clean up
# unlink("spe9hm", recursive = TRUE)

#==============================================================================
# misc experimentation
# rm(list = ls())
# #setwd("/home/gerw/gitrepos/runOPM/tests/testthat")
# hvfn <- "/home/gerw/gitrepos/runOPM/tests/testthat/spe9hm/DECKS/SPE9_hmvars.rds"
# spe9vars <- readRDS(hvfn)
#
# mefn <- "/home/gerw/gitrepos/runOPM/tests/testthat/spe9hm/REPORTS/MemberErrorLong.csv"
# spe9_mem_err <- readr::read_csv(mefn)
# member_error <- spe9_mem_err
#
# msfn <- "/home/gerw/gitrepos/runOPM/tests/testthat/spe9hm/REPORTS/MemberSelection.rds"
# spe9_mod_sel <- readRDS(msfn)
# model_selection <- spe9_mod_sel
# optfn <- "/home/gerw/gitrepos/runOPM/tests/testthat/spe9hm/REPORTS/spe_km_opt.rds"
# spe9_opt <- readRDS(optfn)
#
# projsumfn <- "/home/gerw/gitrepos/runOPM/tests/testthat/BIGFILES/PROJSUM.csv"
# spe9rslts <- readr::read_csv(projsumfn, col_types =.LongColSpec())
#
# wide_response <-.Long2WideError(spe9_mem_err, spe9_mod_sel)
# ma <- function(x){mean(abs(x))}
# MeanAbsOpt <- apply(spe9_opt$values,1,ma)
# MeanAbs <- apply(wide_response,1,ma)
# wr <- as.data.frame(cbind(wide_response, MeanAbs))
# MAorder <- order(wr$MeanAbs)
# best10 <- rownames(wide_response)[MAorder][1:10]
#
# runOPM::PlotEach(longdata = spe9rslts, casenames = best10,
#                  wgnames = "FIELD", keywords = c("WOPR", "WGPR", "WWPR"),
#                  basecase = "HIST_CSV")
#
# longdata = spe9rslts
# casenames = best10
# wgnames = "FIELD"
# keywords = c("WOPR", "WGPR", "WWPR")
# vars.long.df <-.BuildPlotFIlter(longdata,
#                                  casenames = casenames, wgnames = wgnames,
#                                  keywords = keywords)
