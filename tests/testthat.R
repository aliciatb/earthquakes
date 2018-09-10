library(testthat)
library(earthquakes)

test_that("eq_location_clean('ITALY: POMPEI') works", {
  expect_equal(eq_location_clean("ITALY","ITALY: POMPEI"),"Pompei")
})
