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
wr_wo_date <- wide_raw[,colnames(wide_raw) != "DATE" & colnames(wide_raw) != "DAYS"]
summary(wr_wo_date)
colsums_wide_raw <- as.matrix(t(colSums(wr_wo_date)))
# calculated with gnumeric
colsums_tsv <-  system.file("testdata", "SPE1_CASE1_ColSums.tsv", package = "runOPM")
colsums_gnumeric <- read.delim(colsums_tsv)
colsums_gnumeric <- as.matrix(colsums_gnumeric[,colnames(colsums_gnumeric) != "DATE"])
#------------------------------------------------------------------------------
test_that(".GetECL with the python dependency works", {
  expect_true(is.data.frame(wide_raw))
  expect_equal(colsums_wide_raw, colsums_gnumeric)
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
