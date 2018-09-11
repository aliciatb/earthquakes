library(testthat)
library(earthquakes)

test_that("eq_clean_data(raw) returns a data frame or tibble", {
  df <- eq_clean_data(raw)
  expect_that(df, is_a("data.frame"))
})

test_that("eq_location_clean('ITALY: POMPEI') works", {
  expect_equal(eq_location_clean("ITALY","ITALY: POMPEI"),"Pompei")
})

