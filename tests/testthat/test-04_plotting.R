context("Preparing to plot")

#==============================================================================
long <- readRDS("results.rds")
vldf <- readRDS("unique.rds")
#------------------------------------------------------------------------------
test_that("plotting helpers work", {
  expect_equal_to_reference(.UniqueVars(long), "unique.rds")
  expect_equal_to_reference(.BuildPLotFIlter(long,
                                             wgnames = c("PROD", "INJ"),
                                             vars.long.df = vldf),
                            "filt_wgn.rds")
  expect_equal_to_reference(.BuildPLotFIlter(long,
                                             keywords = c("WOPR", "WGIR"),
                                             vars.long.df = vldf),
                             "filt_kw.rds")
})
#==============================================================================
#clean up
unlink("testsim", recursive = TRUE)
