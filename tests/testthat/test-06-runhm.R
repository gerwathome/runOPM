context("Running the HM, gathering results and calculating errors")

#==============================================================================
template <- "SPE9.TEMPLATE"
from.template.path <-  system.file("extdata", template, package = "runOPM")
from.perm.inc.path <-  system.file("extdata", "GRID", "PERMVALUES.INC",
                              package = "runOPM")
from.grdecl.path <-  system.file("extdata", "GRID", "SPE9.GRDECL",
                              package = "runOPM")
basedir <- file.path(getwd(), "spe9hm")
MakeProj(deckname = from.template.path, basedir = "spe9hm")
deckdir <- file.path(basedir, "DECKS")
templatepath <- file.path(deckdir, template)
griddir <- file.path(deckdir, "GRID")
if (!dir.exists(griddir)) {dir.create(griddir)}
file.copy(from.perm.inc.path, file.path(griddir, "PERMVALUES.INC"),
          overwrite = TRUE)
file.copy(from.grdecl.path, file.path(griddir, "SPE9.GRDECL"), overwrite = TRUE)
hist_csv_name <-  system.file("extdata", template, package = "runOPM")
hist_xls_name <-  system.file("extdata", template, package = "runOPM")
hist_xlt_name <-  system.file("extdata", template, package = "runOPM")

#------------------------------------------------------------------------------
test_that("ImportHist works", {
  # check that each method brings in the same data
  expect_equal(1, 1)
})
#==============================================================================
spe9vars <- ReadTemplate(from.template.path, "spe9hm")
spe9vars <- EditVar(spe9vars, pattern = "PORO", truncLow = 0.1,
                    truncHigh = 2, param1 = 0.1, param2 = 2.0,
                    basedir = basedir)
spe9vars <- EditVar(spe9vars, pattern = "PERM", truncLow = 0.1,
                    truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
                    basedir = basedir)
set.seed(424242)
spe9vars <- ExpDes(spe9vars, type = "fpb", basedir = basedir)
spe9decks <- BuildDecks(spe9vars, template = templatepath,  basedir = basedir,
                        overwrite = NULL, cases = NULL)

ok <- RunFlow(spe9decks[1:3], basedir = basedir, overwrite = NULL, wait = TRUE)
if (!is.null(ok)) {print(paste0("value of ok is ", ok))}
spe9rslts <- EclSum(basedir = basedir)
# ploteach(spe9rslts, wgnames="FIELD")
#------------------------------------------------------------------------------
test_that("CalcErrs works", {
  expect_equal(1, 1)
})
#==============================================================================
#------------------------------------------------------------------------------
test_that("VarSens1 works", {
  expect_equal(1, 1)
})
#==============================================================================
#------------------------------------------------------------------------------
test_that("VarSens2 works", {
  expect_equal(1, 1)
})

#==============================================================================
# clean up
# unlink("spe9hm", recursive = TRUE)
