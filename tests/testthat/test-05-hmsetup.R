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
# These parameters are multipliers, not absolute values
# A value of 1 would give a model equivalent to the unmodified SPE9 case
# Assume that our estimates of porosity are a little low so that the mean value
#   of the multiplier is below 1: low = 0.1
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
pattern <- "^\\-\\- (\\w+) = (.+)$"
vars07lines <- grep(pattern, readLines( spe9decks[7]),
                    perl = TRUE, value = TRUE)
varnames <- gsub(pattern, "\\1", vars07lines, perl = TRUE)
varvals <- as.numeric(gsub(pattern, "\\2", vars07lines, perl = TRUE))
names(varvals) <- varnames
testvals <- spe9vars$expDesignUncoded[7,varnames]
#------------------------------------------------------------------------------
test_that("BuildDecks works", {
  expect_identical(spe9decks,
                   list.files(deckdir, "SPE9_.+\\.DATA",
                              full.names = TRUE, include.dirs = TRUE))
  expect_equal(varvals, testvals)
})
#==============================================================================
lc <- -1
hc <- 1
lu <- 0.2
hu <- 200
coded <- runif(10, min = lc, max = hc)
c2u <- .Coded2Uncoded(coded = coded, lu = lu, hu = hu, lc = lc, hc = hc)
back2c <- .Uncoded2Coded(uncoded = c2u, lu = lu, hu = hu, lc = lc, hc = hc)
uncoded <- runif(10, min = lu, max = hu)
u2c <- .Uncoded2Coded(uncoded = uncoded, lu = lu, hu = hu, lc = lc, hc = hc)
back2u <- .Coded2Uncoded(coded = u2c, lu = lu, hu = hu, lc = lc, hc = hc)
#------------------------------------------------------------------------------
test_that("coded / uncoded functions work", {
  expect_equal(coded, back2c)
  expect_equal(uncoded, back2u)
  expect_error(.Coded2Uncoded(coded = coded, lu = lu, hu = hu, lc = 1, hc = -1),
               "low coded")
  expect_error(.Coded2Uncoded(coded = coded, lu = 100, hu = 1, lc = lc, hc = nc),
               "low uncoded")
  expect_error(.Uncoded2Coded(uncoded = uncoded, lu = lu, hu = hu, lc = 1, hc = -1),
               "low coded")
  expect_error(.Uncoded2Coded(uncoded = uncoded, lu = 100, hu = 1, lc = lc, hc = nc),
               "low uncoded")
})
#==============================================================================
# defer clean up so that the following tests may use spe9hm data
# clean up
# unlink("spe9hm", recursive = TRUE)
