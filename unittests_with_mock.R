library(testthat)
library(webmockr)
source("/Users/somyanagar/Desktop/Workspace/Projects/API-Shows/shows_package/shows.R")

# Turned on mocks
httr_mock(on = TRUE)

get_episodes_of_season_mock_json <- '[{"id":1,"url":"https://www.tvmaze.com/episodes/1/under-the-dome-1x01-pilot","name":"Pilot","season":1,"number":1,"type":"regular","airdate":"2013-06-24","airtime":"22:00","airstamp":"2013-06-25T02:00:00+00:00","runtime":60,"rating":{"average":6.8},"image":{"medium":"https://static.tvmaze.com/uploads/images/medium_landscape/1/4388.jpg","original":"https://static.tvmaze.com/uploads/images/original_untouched/1/4388.jpg"},"summary":"<p>When the residents of Chester\'s Mill find themselves trapped under a massive transparent dome with no way out, they struggle to survive as resources rapidly dwindle and panic quickly escalates.</p>","_links":{"self":{"href":"https://api.tvmaze.com/episodes/1"},"show":{"href":"https://api.tvmaze.com/shows/1"}}}]'
get_shows_mock_json_data <- '[{"score":0.9095458,"show":{"id":139,"url":"https://www.tvmaze.com/shows/139/girls","name":"Girls","type":"Scripted","language":"English","genres":["Drama","Romance"],"status":"Ended","runtime":30,"averageRuntime":30,"premiered":"2012-04-15","ended":"2017-04-16","officialSite":"http://www.hbo.com/girls","schedule":{"time":"22:00","days":["Sunday"]},"rating":{"average":6.5},"weight":97,"network":{"id":8,"name":"HBO","country":{"name":"United States","code":"US","timezone":"America/New_York"},"officialSite":"https://www.hbo.com/"},"webChannel":null,"dvdCountry":null,"externals":{"tvrage":30124,"thetvdb":220411,"imdb":"tt1723816"},"image":{"medium":"https://static.tvmaze.com/uploads/images/medium_portrait/31/78286.jpg","original":"https://static.tvmaze.com/uploads/images/original_untouched/31/78286.jpg"},"updated":1704794122,"_links":{"self":{"href":"https://api.tvmaze.com/shows/139"},"previousepisode":{"href":"https://api.tvmaze.com/episodes/1079686"}}}}]'

stub_request('get', uri = 'https://api.tvmaze.com/seasons/1/episodes') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = get_episodes_of_season_mock_json,
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  )

stub_request('get', uri = 'https://api.tvmaze.com/seasons/2/episodes') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = '[]',
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  )

stub_request('get', uri = 'https://api.tvmaze.com/search/shows?q=girls') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = get_shows_mock_json_data,
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  )

stub_request('get', uri = 'https://api.tvmaze.com/search/shows?q=test') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = '[]',
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  )

test_that("get_episodes_of_season returns correct JSON data", {
  season_id <- 1
  result <- get_episodes_of_season(season_id)
  expect_is(result, "data.frame")
})

test_that("get_episodes_of_season returns empty JSON data", {
  season_id <- 2
  result <- get_episodes_of_season(season_id)
  expect_equal(result, "There is no information for this season")
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

test_that("get_shows returns correct JSON data", {
  query <- 'girls'
  result <- get_shows(query)
  expect_is(result, "data.frame")
})

test_that("get_shows returns correct JSON data", {
  query <- 'girls'
  result <- get_shows(query)
  expect_is(result, "data.frame")
})

test_that("get_shows returns correct empty data", {
  query <- 'test'
  result <- get_shows(query)
  expect_null(result)
})

test_that("format_show_name returns a dataframe with specific columns", {
  mock_response = list(
    name = "Girls",
    premiered = "1",
    ended = "1",
    genres = "A, B"
  )
  result <- format_show_name(mock_response)
  expect_is(result, "data.frame")
  expect_true("name" %in% names(result))
})