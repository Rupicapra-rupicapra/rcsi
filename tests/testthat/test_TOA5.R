library(rcsi)
context("Importing TOA5 data files")

header <- importTOA5data("TOA5TestSet.dat", RetOpt = "info")
tdata <- importTOA5data("TOA5TestSet.dat")

test_that("TOA5 header import gives the right informations", {
  expect_equal(header[1], "TOA5,CR1000,CR1000,1031,CR1000.Std.00.60,CPU:Test.CR1,4062,Test")
  expect_equal(header[2], "TIMESTAMP,RECORD,batt_volt_Min,PTemp")
  expect_equal(header[3], "TS,RN,Volts,C")
  expect_equal(header[4], ",,Min,Smp")
}
)

test_that("TOA5 data import gives the right data", {
  expect_equal(tdata[1,3], 13.7)
  expect_equal(tdata[2,2], 1)
  expect_equal(tdata[3,4], 24.98)
})