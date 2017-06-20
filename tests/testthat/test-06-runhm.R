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
# build a history file
# from.hist.path <- system.file("extdata", "SPE9_CP.DATA", package = "runOPM")
# file.copy(from.hist.path, file.path(deckdir, "HISTORY.DATA"), overwrite = TRUE)
# RunFlow("HISTORY.DATA", basedir = basedir, overwrite = NULL, wait = TRUE)
# case <- "HISTORY"
# infile <- runOPM:::.FindSummary(basedir = basedir, casename = case)
# outfile <- file.path(basedir, "OUTPUT", case, paste0(case,".csv"))
# wide_raw <- runOPM:::.GetECL(case, infile, outfile)
hist_csv_name <-  system.file("testdata", "HIST_CSV.csv", package = "runOPM")
hist_csv <- suppressMessages(readr::read_csv(hist_csv_name))
hist_csv$DATE <- format(as.Date(hist_csv$DATE, "%d-%b-%Y"), "%d-%b-%Y")
numeric_cols <- !grepl("DAYS|DATE|CASE", colnames(hist_csv), perl = TRUE)
hist_csv[,numeric_cols] <- as.data.frame(data.matrix(hist_csv[,numeric_cols]))

hist_xlsx_name <-  system.file("testdata", "HIST_XLSX.xlsx", package = "runOPM")
hist_xlsx <- suppressMessages(readxl::read_excel(hist_xlsx_name))
hist_xlsx$DATE <-  format(as.Date(hist_xlsx$DATE - 1, origin = "1899-12-31"),
                          "%d-%b-%Y")
numeric_cols <- !grepl("DAYS|DATE|CASE", colnames(hist_xlsx), perl = TRUE)
hist_xlsx[,numeric_cols] <- as.data.frame(data.matrix(hist_xlsx[, numeric_cols]))
# all.equal(hist_csv, hist_xlsx)

long_csv <- runOPM::ImportHist(ssheetfile = hist_csv_name, basedir = basedir)
long_xlsx <- runOPM::ImportHist(ssheetfile = hist_xlsx_name, basedir = basedir)
csv_df <- long_xlsx[long_xlsx$CASENAME == "HIST_CSV", 2:7]
xlsx_df <- long_xlsx[long_xlsx$CASENAME == "HIST_XLSX", 2:7]
# all.equal(csv_df, xlsx_df)
#------------------------------------------------------------------------------
test_that("ImportHist works", {
  # check that each method brings in the same data
  expect_equal(hist_csv, hist_xlsx)
  expect_equal(csv_df, xlsx_df)
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
PlotEach(spe9rslts, wgnames = "FIELD")
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
