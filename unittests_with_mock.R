library(testthat)
library(webmockr)
source("/Users/somyanagar/Desktop/Workspace/Projects/API-Shows/shows_package/shows.R")

# Turned on mocks
httr_mock(on = TRUE)

get_episodes_of_season_mock_json <- '[{"id":1,"url":"https://www.tvmaze.com/episodes/1/under-the-dome-1x01-pilot","name":"Pilot","season":1,"number":1,"type":"regular","airdate":"2013-06-24","airtime":"22:00","airstamp":"2013-06-25T02:00:00+00:00","runtime":60,"rating":{"average":6.8},"image":{"medium":"https://static.tvmaze.com/uploads/images/medium_landscape/1/4388.jpg","original":"https://static.tvmaze.com/uploads/images/original_untouched/1/4388.jpg"},"summary":"<p>When the residents of Chester\'s Mill find themselves trapped under a massive transparent dome with no way out, they struggle to survive as resources rapidly dwindle and panic quickly escalates.</p>","_links":{"self":{"href":"https://api.tvmaze.com/episodes/1"},"show":{"href":"https://api.tvmaze.com/shows/1"}}}]'

stub_request('get', uri = 'https://api.tvmaze.com/seasons/1/episodes') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = get_episodes_of_season_mock_json,
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  ) 

test_that("get_episodes_of_season returns correct JSON data", {
  season_id <- 1
  result <- get_episodes_of_season(season_id)
  expect_is(result, "data.frame")
})

test_that("format_episode_name returns a dataframe with specific columns", {
  mock_response = list(
    name = "Lord Snow",
    season = "1",
    number = "1",
    rating = "8.1"
  )
  result <- format_episode_name(mock_response)
  expect_is(result, "data.frame")
  expect_true("Episode" %in% names(result))
  expect_true("Name" %in% names(result))
  })