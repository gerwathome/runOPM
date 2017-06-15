context("Create project directories")

# make sure we are ready to start
if (dir.exists("testdir")) {
  unlink("testdir", recursive = TRUE)
}
#==============================================================================
projdirs <- c(".", "./testdir", "./testdir/DECKS", "./testdir/OUTPUT",  "./testdir/REPORTS")
#------------------------------------------------------------------------------
test_that("make project works", {
  expect_warning(MakeProj("not_here.txt", "testdir"),"Failed to find deck")
  expect_equal(list.dirs(recursive = TRUE), projdirs)
})
# clean up
unlink("testdir", recursive = TRUE)

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
context("Read data deck")

# ulugg:~/gitrepos/runOPM/inst/extdata> egrep -A1 INCLUDE SPE9.DATA | egrep -v INCLUDE | egrep -v '\-\-'
# 	TOPSVALUES.DATA /
# 	PERMVALUES.DATA /
# ulugg:~/gitrepos/runOPM/inst/extdata> wc -l *.DATA
#   2290 PERMVALUES.INC + 1 because last line is missing \n, which is what wc counts
#    412 SPE1_CASE1.DATA
#    538 SPE9.DATA
#    704 TOPSVALUES.INC
#   3944 total
#==============================================================================
# deck with several mults on same line
deck1 <-  system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
# deck with include files
deck2 <-  system.file("extdata", "SPE9.DATA", package = "runOPM")
dl1 <- .GetDeckLines(deck1)
dl2 <- .GetDeckLines(deck2)
#------------------------------------------------------------------------------
test_that(".GetDeckLines works with and without INCLUDE", {
  expect_equal(length(dl1), 412)
  # adding includes means dropping the INCLUDE line and the filename line
  expect_equal(length(dl2), 538 + 704 + 2290 + 1 - 2 - 2)
})
#==============================================================================
poro1 <- .GetDataVec("PORO", dl1)
perm1 <- .GetDataVec("PERMX", dl1)
poro2 <- .GetDataVec("PORO", dl2)
perm2 <- .GetDataVec("PERMX", dl2)
griddims1 <- as.numeric(.GetDataVec("DIMENS", dl1))
griddims2 <- as.numeric(.GetDataVec("DIMENS", dl2))

sumporo1 <- sum(300*0.3)
sumperm1 <- sum(c(100*500, 100*50, 100*200))
sumporo2 <- sum(c(600*0.087, 600*0.097, 600*0.111, 600*0.16, 600*0.13, 600*0.17,
                  600*0.17, 600*0.08, 600*0.14, 600*0.13, 600*0.12, 600*0.105,
                  600*0.12, 600*0.116, 600*0.157))
perm2txt <- system.file("testdata", "PERMVALUES.txt", package = "runOPM")
sumperm2 <- sum(as.numeric(readLines(perm2txt)))
#------------------------------------------------------------------------------
test_that(".GetDataVec works for array and multipliy type data", {
  expect_equal(prod(griddims1), length(poro1))
  expect_equal(prod(griddims1), length(perm1))
  expect_equal(prod(griddims2), length(poro2))
  expect_equal(prod(griddims2), length(perm2))
  expect_equal(sum(poro1), sumporo1)
  expect_equal(sum(perm1), sumperm1)
  expect_equal(sum(poro2), sumporo2)
  expect_equal(sum(perm2), sumperm2)
})
#==============================================================================
test_that(".ExpandVals works", {
  expect_equal(.ExpandVals("10*7"), rep(7, 10))
  expect_equal(.ExpandVals("5*0.3"), rep(0.3, 5))
})
#==============================================================================
df1 <- ReadDeck(deck1, c("PORO", "PERMX"))
df2 <- ReadDeck(deck2, c("PORO", "PERMX"))
#------------------------------------------------------------------------------
test_that("ReadDeck works for multiple properties", {
  expect_equal(sum(df1[,"PORO"]), sumporo1)
  expect_equal(sum(df1[,"PERMX"]), sumperm1)
  expect_equal(sum(df2[,"PORO"]), sumporo2)
  expect_equal(sum(df2[,"PERMX"]), sumperm2)
})
