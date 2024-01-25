library(httr)
library(jsonlite)
library(glue)
library(roxygen2)

BASE_URL = "https://api.tvmaze.com/"

#' get_shows
#'
#' @param query 
#'
#' @return a dataframe containing all information on a given television show
#' @export
#'
#' @examples get_shows("Hello Kitty")
get_shows <- function(query){
  query = paste0(BASE_URL, "search/shows?q=", URLencode(query))
  response = GET(query)
  json_content = content(response, "text", encoding = "UTF-8")
  parse_json = fromJSON(json_content)$show
  ifelse(length(parse_json) == 0, return (NULL), return (parse_json))
}

#' format_show_name
#'
#' @param show 
#'
#' @return a dataframe of the shows returned in the get_shows() function, along 
#' with premier date, end date, and the genres
#' @export
#'
#' @examples format_show_name(get_shows("Hello Kitty"))
format_show_name <- function(show){
  
  genres = character()
  for (i in 1: length(show$genres)){
    genres[i] = paste(unlist(show$genres[i]), collapse = ", ")
  }
  
  df_shows <- data.frame(
    "name" = show$name,
    "premiered" = show$premiered,
    "ended" = show$ended,
    "genres" = genres
  )
  
  
  df_shows <- data.frame(apply(df_shows, c(1, 2), 
                               function(x) ifelse(is.na(x) | x == " ", "?", x)))
  df_shows[] <- apply(df_shows, 2, function(x) ifelse(trimws(as.character(x)) == "", "?", x))
  
  df_shows$premiered = gsub("-.*", "", df_shows$premiered)
  df_shows$ended = gsub("-.*", "", df_shows$ended)
  
  return (df_shows)
}


#' get_seasons
#'
#' @param show_id 
#'
#' @return parsed json file containing information about the seasons
#' @export
#'
#' @examples get_seasons(1505)
get_seasons <- function(show_id){
  
  link <- paste0(BASE_URL, "shows/", as.character(show_id), "/seasons")
  response <- GET(link)
  json_content <- content(response, "text", encoding = "UTF-8")
  parsed_json <- fromJSON(json_content)
  parsed_json <- data.frame(apply(parsed_json, c(1, 2), function(x) ifelse(is.na(x) | x == " ", "?", x)))
  return(parsed_json)
  
}

#' format_season_name
#'
#' @param season 
#'
#' @return a data frame that has been cleaned of NA values
#' @export
#'
#' @examples format_season_name(season)
format_season_name <- function(season){
  
  # making the data frame
  formatted_seasons <- data.frame(Season = glue("Season {season$number}"), Name = season$name, Premier = glue("{substr(season$premiereDate, 1, 4)}"), 
                                  End = glue("{substr(season$endDate, 1, 4)}"), Episodes = season$episodeOrder)
  
  # replacing NA values with ? to account for missing information in the API
  formatted_seasons[is.na(formatted_seasons)] <- "?"
  return(formatted_seasons)
}

get_episodes_of_season <- function(season_id) {
  link <- paste0(BASE_URL, "seasons/", as.character(season_id), "/episodes")
  response <- GET(link)
  json_content <- content(response, "text", encoding = "UTF-8")
  parsed_json <- fromJSON(json_content)
  return(parsed_json)
}

format_episode_name <- function(episode) {
  number <- lapply(episode$number, replace_na)
  rating <- lapply(episode$rating, replace_na)
  
  ep_number <- paste0("S", episode$season, "E", number)
  df <- data.frame('Episode' = ep_number, 'Name' = episode$name, 'Rating' = rating)
  return(df)
}

replace_na <- function(x) {
  x[is.na(x)] <- "?"
  return(x)
}
main <- function(){
  exit <- FALSE
  while (exit == FALSE) {
    query <- readline("Enter a Show (or 0 to exit): ")
    if (query == "0") {
      break
    }
    results <- get_shows(query)
    
    if(is.null(results)) {
      cat("No results found")
    } else {
      cat("Here are the results:\n")
      details <- format_show_name(results)
      print(details)
      
      seasons_input <- readline("Select a Show number (or 0 to exit): ")
      if (seasons_input == "0") {
        break
      }
      index_seasons <- as.numeric(seasons_input)
      season_id <- results$id[index_seasons]
      
      seasons <- get_seasons(season_id)
      season_names <- format_season_name(seasons)
      print(season_names)
      
      season_number_input <- as.numeric(readline("Select a Show number (or 0 to exit): "))
      if (season_number_input == "0") {
        break
      }
      
      season_id <- trimws(seasons$id[season_number_input])
      episodes <- get_episodes_of_season(season_id)
    
      episode_details <- format_episode_name(episodes)
      new_col_names <- c("Episode", "Name", "Rating")
      
      colnames(episode_details) <- new_col_names
      print(episode_details)
    }
  }
}

main()
