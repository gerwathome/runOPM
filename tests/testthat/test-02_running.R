context("Run simulation model")

# make sure we are ready to start
if (dir.exists("testdir")) {
  unlink("testdir", recursive = TRUE)
}
if (dir.exists("testsim")) {
  unlink("testsim", recursive = TRUE)
}
if (dir.exists("spe9hm")) {
  unlink("spe9hm", recursive = TRUE)
}

# this needs a lot more testing, eventually
#==============================================================================
projdirs1 <- c(".", "./testsim", "./testsim/DECKS", "./testsim/OUTPUT",
              "./testsim/OUTPUT/SPE1_CASE1","./testsim/REPORTS")
deck1 <-  system.file("extdata", "SPE1_CASE1.DATA", package = "runOPM")
print(list.dirs(recursive = TRUE))
#------------------------------------------------------------------------------
test_that("RunFlow works", {
  expect_error(RunFlow("not_here.data", "testsim"),"Failed to find ")
  expect_error(RunFlow(deck1, "testsim"),NA)
  expect_equal(list.dirs(recursive = TRUE), projdirs1)
  expect_error(RunFlow(deck1, "testsim", sim_exec = "noexec"),
               "Failed to locate ")
})
# clean up
unlink("testsim", recursive = TRUE)
