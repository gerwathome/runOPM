context("Running the HM, gathering results and calculating errors")

#==============================================================================
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
spe9vars <- EditVar(spe9vars, pattern = "PORO", truncLow = 0.1,
                     truncHigh = 2, param1 = 0.1, param2 = 2.0,
                     basedir = basedir)
spe9vars <- EditVar(spe9vars, pattern = "PERM", truncLow = 0.1,
                     truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
                     basedir = basedir)
 set.seed(424242)
 spe9vars <- ExpDes(spe9vars, edtype = "augfpb", basedir = basedir)
 spe9decks <- BuildDecks(spe9vars, template = templatepath,  basedir = basedir,
                         overwrite = NULL, cases = NULL)
 # add hist data to PROJSUM.csv file
 hist_csv_name <-  system.file("testdata", "HIST_CSV.csv", package = "runOPM")
 long_csv <- ImportHist(ssheetfile = hist_csv_name, basedir = basedir)
# this runs the augment 300 case deck list
ok <- RunFlow(spe9decks, basedir = basedir, overwrite = NULL, wait = TRUE)
if (!is.null(ok)) {print(paste0("value of RunFlow ok is ", ok))}
spe9rslts <- EclSum(basedir = basedir)

# projsumfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
# spe9rslts <- readr::read_csv(projsumfn, col_types = runOPM:::.LongColSpec())
#PlotEach(spe9rslts, wgnames = "FIELD")
long <- CalcErrors(long = spe9rslts, base_case = "HIST_CSV",
                               basedir = basedir)

# psfn <- file.path(basedir, "REPORTS", "PROJSUM.csv")
# long <- readr::read_csv(psfn, col_types = runOPM:::.LongColSpec())
element_error <- ErrorByElement(long = long, basedir = basedir)
# eefn <- file.path(basedir, "REPORTS", "ElementError.csv")
# eesum <- readr::read_csv(eefn, col_types = runOPM:::.ErrorByElementColSpec())
# head(element_error)
# summary(element_error)
member_error <- ErrorByMember(long = long, basedir = basedir)
spe9_mem_err <- member_error
# mefn <- file.path(basedir, "REPORTS", "MemberErrorLong.csv")
# member_error <- readr::read_csv(mefn, col_types = runOPM:::.ErrorByElemLongColSpec())
# head(member_error)
# summary(member_error)

#------------------------------------------------------------------------------
test_that("Error calculation and summarizing works", {
  expect_equal_to_reference(spe9vars$expDesignCoded,
                            "spe9vars_expdes_coded.rds")
  expect_equal_to_reference(long, "test_calc_errors.rds")
  expect_equal_to_reference(element_error, "test_element_errors.rds")
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

spe.km <- BuildKModels(hmvars = spe9vars, member_error = member_error,
                       model_selection = spe9_mod_sel, basedir = "spe9hm")
#------------------------------------------------------------------------------
test_that("BuildKModels works", {
  expect_equal(1, 1)
})

#==============================================================================
spe9_varsens <- ModelSensitivity(hmvars = spe9vars, member_error = spe9_mem_err,
                                 model_selection = spe9_mod_sel)
#------------------------------------------------------------------------------
test_that("ModelSensitivity works", {
  expect_equal(1, 1)
})
#==============================================================================
spe9_opt <- RunGPareto(kmodels = spe.km, method = "genoud", basedir = "spe9hm")
#------------------------------------------------------------------------------
test_that("RunGPareto works", {
  expect_equal(1, 1)
})

#==============================================================================

#------------------------------------------------------------------------------
test_that("More stuff works", {
  expect_equal(1, 1)
})

#==============================================================================
# clean up
# unlink("spe9hm", recursive = TRUE)
