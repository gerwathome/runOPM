context("Reading the ECL Summary file")

# make sure we are ready to start
if (dir.exists("testsim")) {
  unlink("testsim", recursive = TRUE)
}
#==============================================================================
deck1 <-  system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
deck2 <-  system.file("extdata", "SPE9.DATA", package = "runOPM")
inc1 <-  system.file("extdata", "GRID", "TOPSVALUES.INC", package = "runOPM")
inc2 <-  system.file("extdata", "GRID", "PERMVALUES.INC", package = "runOPM")
#------------------------------------------------------------------------------
test_that("Keyword labeling works", {
  expect_equal(.KW2Descrip("WOPR"), "Oil Prod Rate")
  expect_equal(.KW2Descrip("NotAKeyword"), "Unknown Parameter")
  expect_equal(.KW2Units("WOPR"), "STBD")
  expect_equal(.KW2Units("NotAKeyword"), "")
  expect_equal(.FindUnitType(deck1), "FIELD")
  expect_equal(.KW2Label("WOPR"), "Oil Prod Rate, STBD")
  expect_equal(.KW2Label("NotAKeyword"), "NotAKeyword")
  expect_equal(.WGN_KW2Title("PROD", "WOPR"), "PROD:  Oil Prod Rate")
  expect_equal(.WGN_KW2Title("PROD", "NotAKeyword"), "PROD:  NotAKeyword")
})
#==============================================================================
RunFlow(deck1, basedir = "testsim", wait = TRUE)
deckdir <- file.path("testsim","DECKS")
griddir <- file.path(deckdir,"GRID")
if (!dir.exists(griddir)) {dir.create(griddir)}
file.copy(inc1, griddir)
file.copy(inc2, griddir)
# This is very slow; consider simplfying the SPE9 deck
RunFlow(deck2, basedir = "testsim", wait = TRUE)
#------------------------------------------------------------------------------
test_that("Finding files works", {
  expect_equal(basename(.FindDecks("testsim", casename = "SPE1_CASE1")),
               "SPE1_CASE1.DATA")
  expect_equal(basename(.FindDecks("testsim", casename = "SPE9")),
               "SPE9.DATA")
  expect_equal(basename(.FindDecks("testsim")),
               c("SPE1_CASE1.DATA", "SPE9.DATA"))
  expect_equal(basename(.FindSummary("testsim", casename = "SPE1_CASE1")),
               "SPE1_CASE1.UNSMRY")
  expect_equal(basename(.FindSummary("testsim", casename = "SPE9")),
               "SPE9.UNSMRY")
  expect_equal(basename(.FindSummary("testsim")),
               c("SPE1_CASE1.UNSMRY", "SPE9.UNSMRY"))
})
#==============================================================================
case <- "SPE1_CASE1"
basedir <- normalizePath("testsim")
infile <- .FindSummary(basedir, casename = "SPE1_CASE1")
outfile <- file.path(basedir, "OUTPUT", case, paste0(case,".csv"))
wide_raw <- .GetECL(case, infile, outfile)
#------------------------------------------------------------------------------
test_that(".GetECL with the python dependency works", {
  expect_true(is.data.frame(wide_raw))
  # enough timesteps
  expect_gte(nrow(wide_raw), 120)
  # initial opr
  expect_equal(wide_raw[1,"FOPR"], 20000)
  # final oil cum within 1%
  expect_lte(abs((wide_raw[nrow(wide_raw),"WOPT.PROD"] - 52282200)
                 / 52282200), 0.01)
  # final gas cum within 1%
  expect_lte(abs((wide_raw[nrow(wide_raw),"WGPT.PROD"] - 339460000)
                 / 339460000), 0.01)
  # final water cum less than 1
  expect_lte(wide_raw[nrow(wide_raw), "WWPT.PROD"], 1)
  # final bhp at limiting constraint
  expect_equal(wide_raw[nrow(wide_raw), "WBHP.PROD"], 1000)
})
#==============================================================================
wide <- .CleanWide(case, wide_raw)
#------------------------------------------------------------------------------
test_that(".CleanWide works", {
  expect_true(is.data.frame(wide))
  expect_equal(class(wide$CASE), "character")
  expect_equal(class(wide$DATE), "Date")
  expect_equal(length(wide$DAYS), length(wide_raw$DAYS))
})
#==============================================================================
long <- .Wide2Long(wide)
#------------------------------------------------------------------------------
test_that(".Wide2Long works", {
  expect_true(is.data.frame(long))
  expect_equal(class(long$DATE), "Date")
  expect_equal(class(long$CASE), "character")
  expect_equal(class(long$VALUE), "numeric")
  expect_equal(length(long$DATE), length(wide$DATE) * (ncol(wide) - 3))
  # need to think of a way to be sure long and wide values are the same
})
#==============================================================================
results <- EclSum(casename = case, basedir = basedir)
#------------------------------------------------------------------------------
test_that("EclSum works", {
  expect_true(is.data.frame(results))
  expect_equal(class(results$DATE), "Date")
  expect_equal(class(results$CASE), "character")
  expect_equal(class(results$VALUE), "numeric")
  expect_equal_to_reference(results, "results.rds")
})
#==============================================================================
# defer clean up so that following tests may use sim data
# unlink("testsim", recursive = TRUE)
