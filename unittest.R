library(testthat)
setwd("~/Documents/MDS/block4/Data-534/API-Shows")
source("shows_package/shows.R")

test_that("get_shows returns a dataframe", {
  result = get_shows("One Piece")
  expect_is(result, "data.frame")
})

test_that("format_show_name returns a dataframe with specific columns", {
  dummy_show = list(
    name = "Hello Kitty",
    premiered = "2000-01-01",
    ended = "2010-01-01",
    genres = list("Action", "Adventure")
  )
  result <- format_show_name(dummy_show)
  expect_is(result, "data.frame")
  expect_true("name" %in% names(result))
  expect_true("premiered" %in% names(result))
  })
