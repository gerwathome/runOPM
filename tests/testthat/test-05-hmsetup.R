context("Setting up for the history match")

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
spe9vars <- ReadTemplate(from.template.path, "spe9hm")
#------------------------------------------------------------------------------
test_that("ReadTemplate works", {
  expect_s3_class(spe9vars, "hmvars")
  expect_equal_to_reference(spe9vars$vars$name, "spe9vars_name.rds")
})
#==============================================================================
spe9vars <- EditVar(spe9vars, pattern = "PORO", truncLow = 0.1,
                    truncHigh = 2, param1 = 0.1, param2 = 2.0,
                    basedir = basedir)
spe9vars <- EditVar(spe9vars, pattern = "PERM", truncLow = 0.1,
                    truncHigh = 1.5, param1 = 0.1, param2 = 2.0,
                    basedir = basedir)
#------------------------------------------------------------------------------
test_that("EditVar works", {
  expect_s3_class(spe9vars, "hmvars")
  expect_equal_to_reference(spe9vars$vars, "spe9vars_vars.rds")
})
#==============================================================================
# this will take more testing when there are more type choices
# also need to test coded / uncoded translation
set.seed(424242)
spe9vars <- ExpDes(spe9vars, type = "fpb", basedir = basedir)
#------------------------------------------------------------------------------
test_that("ExpDes works", {
  expect_equal_to_reference(spe9vars$expDesignCoded,
                            "spe9vars_expdes_coded.rds")
  expect_equal_to_reference(spe9vars$expDesignUncoded,
                            "spe9vars_expdes_uncoded.rds")
})
#==============================================================================
spe9decks <- BuildDecks(spe9vars, template = templatepath,  basedir = basedir,
                        overwrite = NULL, cases = NULL)
#------------------------------------------------------------------------------
test_that("BuildDecks works", {
  expect_identical(spe9decks,
                  list.files(deckdir, "SPE9_.+\\.DATA",
                             full.names = TRUE, include.dirs = TRUE))
})
#==============================================================================
# defer clean up so that the following tests may use sim data
# clean up
# unlink("spe9hm", recursive = TRUE)
