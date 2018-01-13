context("Setting up for the history match")

#==============================================================================
#setwd("/home/gerw/gitrepos/runOPM/tests/testthat")
template <- "SPE9.TEMPLATE"
from.template.path <-  system.file("extdata", template, package = "runOPM")
from.perm.inc.path <-  system.file("extdata", "GRID", "PERMVALUES.INC",
                              package = "runOPM")
from.grdecl.path <-  system.file("extdata", "GRID", "SPE9.GRDECL",
                              package = "runOPM")
basedir <- file.path(getwd(), "spe9hm")
ok <- MakeProj(deckname = from.template.path, basedir = basedir)
deckdir <- file.path(basedir, "DECKS")
templatepath <- file.path(deckdir, template)
griddir <- file.path(deckdir, "GRID")
if (!dir.exists(griddir)) {dir.create(griddir)}
ok <- file.copy(from.perm.inc.path, file.path(griddir, "PERMVALUES.INC"),
          overwrite = TRUE)
ok <- file.copy(from.grdecl.path, file.path(griddir, "SPE9.GRDECL"),
                overwrite = TRUE)
#spe9vars <- ReadTemplate(from.template.path, "spe9hm")
spe9vars <- ReadTemplate(from.template.path, "spe9hm")
#------------------------------------------------------------------------------
test_that("ReadTemplate works", {
  expect_s3_class(spe9vars, "hmvars")
  expect_equal_to_reference(spe9vars$vars$name, "spe9vars_name.rds")
})
#==============================================================================
# These parameters are multipliers, not absolute values
# A value of 1 would give a model equivalent to the unmodified SPE9 case
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
#------------------------------------------------------------------------------
test_that("EditVar works", {
  expect_s3_class(spe9vars, "hmvars")
  expect_equal_to_reference(spe9vars$vars, "spe9vars_vars.rds")
})
#==============================================================================
# this will take more testing when there are more type choices
# also need to test coded / uncoded translation
# print(paste0("ncases should be 0:  ", nrow(spe9vars$expDesignCoded)))
set.seed(424242)
spe9vars <- ExpDes(spe9vars, edtype = "fpb", basedir = basedir)
# print(paste0("ncases should be 64:  ", nrow(spe9vars$expDesignCoded)))
#------------------------------------------------------------------------------
test_that("ExpDes works", {
  expect_equal_to_reference(spe9vars$expDesignCoded,
                            "spe9vars_expdes_coded_8.rds")
  expect_equal_to_reference(spe9vars$expDesignUncoded,
                            "spe9vars_expdes_uncoded.rds")
})
#==============================================================================
ndesvars <- nrow(spe9vars$designVars)   #  3
ncases <- nrow(spe9vars$expDesignCoded) #  8
nvars <- ncol(spe9vars$expDesignCoded)  # 30
totcases <- 10 * ndesvars               # 30
newcases <- totcases - ncases           # 30 - 8 = 22
spe9vars <- AugExpDes(spe9vars, ncases = newcases, basedir = basedir)
design_lines <- nrow(spe9vars$expDesignCoded) # 30
#------------------------------------------------------------------------------
test_that("AugExpDes works", {
  expect_equal(ncases, 8)
  expect_equal(ndesvars, 3)
  expect_equal(nvars, 30)
  expect_equal(totcases, 30)
  expect_equal(newcases, 22)
  expect_equal(design_lines, 30)
  expect_equal_to_reference(spe9vars$expDesignCoded,
                            "spe9vars_augdes_coded.rds")
  expect_equal_to_reference(spe9vars$expDesignUncoded,
                            "spe9vars_augdes_uncoded.rds")
})
#==============================================================================
# coded_fn <- file.path(getwd(), "spe9vars_augdes_coded.rds")
# coded_df <- readRDS(coded_fn)
# uncoded_fn <- file.path(getwd(), "spe9vars_augdes_uncoded.rds")
# uncoded_df <- readRDS(uncoded_fn)


spe9decks <- BuildDecks(spe9vars, template = templatepath,  basedir = basedir,
                        overwrite = NULL, cases = NULL)
ndecks <- length(spe9decks)
# check that the expected values are in the deck
pattern <- "^\\-\\- (\\w+) = (.+)$"
vars07lines <- grep(pattern, readLines( spe9decks[7]),
                    perl = TRUE, value = TRUE)
varnames <- gsub(pattern, "\\1", vars07lines, perl = TRUE)
varvals <- as.numeric(gsub(pattern, "\\2", vars07lines, perl = TRUE))
names(varvals) <- varnames
testvals <- spe9vars$expDesignUncoded[7,varnames]
#------------------------------------------------------------------------------
test_that("BuildDecks works", {
  expect_equal(ndecks, 30)
  expect_identical(spe9decks,
                   list.files(deckdir, "SPE9_.+\\.DATA",
                              full.names = TRUE, include.dirs = TRUE))
  expect_equal(varvals, testvals)
})
#==============================================================================
set.seed(424242)
lc <- -1
hc <- 1
lu <- 0.2
hu <- 200
coded <- runif(10, min = lc, max = hc)
c2u <- .Coded2Uncoded(coded = coded, lu = lu, hu = hu, lc = lc, hc = hc)
back2c <- .Uncoded2Coded(uncoded = c2u, lu = lu, hu = hu, lc = lc, hc = hc)

luv <- runif(10, min = 0.1, max = 0.2)
huv <- runif(10, min = 200, max = 500)
c2uv <- .Coded2Uncoded(coded = coded, lu = luv, hu = huv, lc = lc, hc = hc)
back2cv <- .Uncoded2Coded(uncoded = c2uv, lu = luv, hu = huv, lc = lc, hc = hc)

uncoded <- runif(10, min = lu, max = hu)
u2c <- .Uncoded2Coded(uncoded = uncoded, lu = lu, hu = hu, lc = lc, hc = hc)
back2u <- .Coded2Uncoded(coded = u2c, lu = lu, hu = hu, lc = lc, hc = hc)
#------------------------------------------------------------------------------
test_that("coded / uncoded functions work", {
  expect_equal(coded, back2c)
  expect_equal(uncoded, back2u)
  expect_equal(coded, back2cv)
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
unlink("spe9hm", recursive = TRUE)

#==============================================================================
# below here is tmp play area that must be deleted
# setwd("/home/gerw/gitrepos/runOPM/tests/testthat")
# test_spe9vars <- readRDS("spe9vars_vars.rds")
# setwd("/home/gerw/gitrepos/runOPM")
