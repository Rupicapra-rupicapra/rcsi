library(rcsi)
context("Importing TOACI1 data files")

header <- importTOACI1data("TOACI1TestSet.dat", RetOpt = "info")
tdata <- importTOACI1data("TOACI1TestSet.dat", RetOpt = "data")

test_that("TOACI1 header import gives the right informations", {
  expect_equal(header[1], "TOACI1,gold,one_min")
  expect_equal(header[2], "TMSTAMP,RECNBR,temp_degf_AVG,meas1,meas2")
}
)

test_that("TOACI1 data import gives the right data", {
  expect_equal(tdata[1,3], 69.05)
})