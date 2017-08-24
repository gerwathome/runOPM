context("Preparing to plot")

#==============================================================================
long <- readRDS("results.rds")
casenames <- c("SPE1_CASE1")
wgnames <- c("FIELD")
keywords <- c("WOPR", "WGPR", "WWPR")
#------------------------------------------------------------------------------
test_that("plotting helpers work", {
  expect_equal_to_reference(.SelectPlotVars(long, casenames, wgnames,
                                            keywords), "spv.rds")
  expect_equal_to_reference(.FilterLong(long, readRDS("spv.rds")), "fl.rds")
  expect_equal_to_reference(.UniqueVars(long), "unique.rds")
  expect_equal_to_reference(.BuildPLotFIlter(long,
                                             wgnames = c("PROD", "INJ"),
                                             vars.long.df = .UniqueVars(long)),
                            "filt_wgn.rds")
  expect_equal_to_reference(.BuildPLotFIlter(long,
                                             keywords = c("WOPR", "WGIR"),
                                             vars.long.df = .UniqueVars(long)),
                             "filt_kw.rds")
})
#==============================================================================
#clean up
unlink("testsim", recursive = TRUE)
