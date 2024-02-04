library(testthat)
library(webmockr)
source("/Users/somyanagar/Desktop/Workspace/Projects/API-Shows/shows_package/shows.R")

httr_mock(on = TRUE)

# get_episodes_of_seasons and format_episode_name testing
get_episodes_of_season_mock_json <- '[{"id":1,"url":"https://www.tvmaze.com/episodes/1/under-the-dome-1x01-pilot","name":"Pilot","season":1,"number":1,"type":"regular","airdate":"2013-06-24","airtime":"22:00","airstamp":"2013-06-25T02:00:00+00:00","runtime":60,"rating":{"average":6.8},"image":{"medium":"https://static.tvmaze.com/uploads/images/medium_landscape/1/4388.jpg","original":"https://static.tvmaze.com/uploads/images/original_untouched/1/4388.jpg"},"summary":"<p>When the residents of Chester\'s Mill find themselves trapped under a massive transparent dome with no way out, they struggle to survive as resources rapidly dwindle and panic quickly escalates.</p>","_links":{"self":{"href":"https://api.tvmaze.com/episodes/1"},"show":{"href":"https://api.tvmaze.com/shows/1"}}}]'

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

test_that("get_episodes_of_season returns empty JSON data", {
  season_id <- 2
  result <- get_episodes_of_season(season_id)
  expect_equal(result, "There is no information for this season")
})

# get seasons and format seasons name testing

get_seasons_mock_json <- '[
  {
    "id": 12403,
    "url": "https://www.tvmaze.com/seasons/12403/one-piece-season-1",
    "number": 1,
    "name": "East Blue Saga",
    "episodeOrder": 61,
    "premiereDate": "1999-10-20",
    "endDate": "2001-03-07",
    "network": {
      "id": 131,
      "name": "Fuji TV",
      "country": {
        "name": "Japan",
        "code": "JP",
        "timezone": "Asia/Tokyo"
      },
      "officialSite": null
    },
    "webChannel": null,
    "image": {
      "medium": "https://static.tvmaze.com/uploads/images/medium_portrait/426/1065875.jpg",
      "original": "https://static.tvmaze.com/uploads/images/original_untouched/426/1065875.jpg"
    },
    "summary": null,
    "_links": {
      "self": {
        "href": "https://api.tvmaze.com/seasons/12403"
      }
    }
  }]'

stub_request('get', uri = 'https://api.tvmaze.com/shows/1505/seasons') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = get_seasons_mock_json,
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  ) 

test_that("get_seasons returns correct JSON data", {
  season_id <- 1505
  result <- get_seasons(season_id)
  expect_is(result, "data.frame")
})

test_that("format_season_name returns a dataframe with specific columns", {
  mock_response = list(
    name = "East Blue Saga",
    episodeOrder = "61",
    number = "1",
    premiereDate = "1999-10-20",
    endDate = "2001-03-07"
  )
  result <- format_season_name(mock_response)
  expect_is(result, "data.frame")
  expect_true("Episodes" %in% names(result))
  expect_true("Name" %in% names(result))
})

get_all_episodes_mock_json <- '[
  {
    "id": 4952,
    "url": "https://www.tvmaze.com/episodes/4952/game-of-thrones-1x01-winter-is-coming",
    "name": "Winter is Coming",
    "season": 1,
    "number": 1,
    "type": "regular",
    "airdate": "2011-04-17",
    "airtime": "21:00",
    "airstamp": "2011-04-18T01:00:00+00:00",
    "runtime": 60,
    "rating": {
      "average": 8.1
    },
    "image": {
      "medium": "https://static.tvmaze.com/uploads/images/medium_landscape/478/1195111.jpg",
      "original": "https://static.tvmaze.com/uploads/images/original_untouched/478/1195111.jpg"
    },
    "summary": "<p>Lord Eddard Stark, ruler of the North, is summoned to court by his old friend, King Robert Baratheon, to serve as the Kings Hand. Eddard reluctantly agrees after learning of a possible threat to the Kings life. Eddards bastard son Jon Snow must make a painful decision about his own future, while in the distant east Viserys Targaryen plots to reclaim his fathers throne, usurped by Robert, by selling his sister in marriage.</p>",
    "_links": {
      "self": {
        "href": "https://api.tvmaze.com/episodes/4952"
      },
      "show": {
        "href": "https://api.tvmaze.com/shows/82"
      }
    }
  }]'

stub_request('get', uri = 'https://api.tvmaze.com/shows/82/episodes') %>%
  wi_th(
    headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
  ) %>%
  to_return(
    body = get_all_episodes_mock_json,
    headers = list('Content-Type' = 'application/json; charset=utf-8')
  ) 

test_that("get_all_episodes returns correct JSON data", {
  season_id <- 82
  result <- get_all_episodes(season_id)
  expect_is(result, "data.frame")
})

test_that("format_all_episodes returns a dataframe with specific columns", {
  mock_response = list(
    name = "Im Luffy! The Man Who Will Become the Pirate King!",
    number = "1",
    rating = "7.8",
    season = "1"
  )
  result <- format_all_episodes(mock_response)
  expect_is(result, "data.frame")
  expect_true("Episode" %in% names(result))
  expect_true("Name" %in% names(result))
  expect_true("Rating" %in% names(result))
})


# get_shows and format_shows_name testing

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

test_that("get_shows returns correct JSON data", {
  query <- 'girls'
  result <- get_shows(query)
  expect_is(result, "data.frame")
})

test_that("get_shows returns empty data", {
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