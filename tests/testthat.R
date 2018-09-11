library(testthat)
library(earthquakes)

test_that("eq_clean_data(raw) returns a data frame or tibble", {
  df <- eq_clean_data(raw)
  expect_that(df, is_a("data.frame"))
})

test_that("eq_location_clean('ITALY: POMPEI') works", {
  expect_equal(eq_location_clean("ITALY","ITALY: POMPEI"),"Pompei")
})

test_that("geom_timeline() returns plot", {
  df <- eq_clean_data(raw)
  plot <- geom_timeline(df, "2000-01-01", "2018-01-01", "NEW ZEALAND")
  expect_that(plot,is_a("ggplot"))
})

test_that("geom_timeline_label() returns plot", {
  df <- eq_clean_data(raw)
  plot <- geom_timeline_label(df, "2000-01-01", "2018-01-01", "NEW ZEALAND", 5)
  expect_that(plot,is_a("ggplot"))
})

test_that("eq_map() returns map", {
  df <- eq_clean_data(raw)
  map <- eq_map(df, "DATE")
  expect_that(map,is_a("leaflet"))
})
