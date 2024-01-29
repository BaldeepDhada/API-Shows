library(httr)
library(jsonlite)
library(glue)
library(roxygen2)
library(ggplot2)
library(tidyverse)
library(dbplyr)

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
  formatted_seasons[is.na(formatted_seasons) | formatted_seasons == ""] <- "?"
  return(formatted_seasons)
}

#' get_episodes_of_season
#'
#' @param season_id 
#'
#' @return a data frame of the parsed json
#' @export
#'
#' @examples get_episodes_of_season(season_id)
get_episodes_of_season <- function(season_id) {
  link <- paste0(BASE_URL, "seasons/", as.character(season_id), "/episodes")
  response <- GET(link)
  json_content <- content(response, "text", encoding = "UTF-8")
  
  if (json_content == "[]") {
    return("There is no information for this season")
  } else {
    parsed_json <- fromJSON(json_content)
    return(parsed_json)
  }
}

#' format_episode_name
#'
#' @param episode 
#'
#' @return a data frame that has been cleaned of NA values
#' @export
#'
#' @examples
format_episode_name <- function(episode) {
  number <- lapply(episode$number, replace_na)
  rating <- lapply(episode$rating, replace_na)
  name <- lapply(episode$name, replace_na)
  
  ep_number <- paste0("S", episode$season, "E", number)
  df <- data.frame('Episode' = ep_number, 'Name' = episode$name, 'Rating' = rating)
  new_col_names <- c("Episode", "Name", "Rating")
  
  colnames(df) <- new_col_names
  return(df)
}

#' replace_na
#'
#' @param x 
#'
#' @return ? if x was NA
#' @export
#'
#' @examples replace_na(x)
replace_na <- function(x) {
  x[is.na(x)] <- "?"
  return(x)
}

#' get_all_episodes
#'
#' @param show_id 
#'
#' @return a data frame of the parsed json of the get response
#' @export
#'
#' @examples get_all_episodes(show_id)
get_all_episodes <- function(show_id) {
  link <- paste0(BASE_URL, "shows/", as.character(show_id), "/episodes")
  response <- GET(link)
  json_content <- content(response, "text", encoding = "UTF-8")
  parsed_json <- fromJSON(json_content)
  return(parsed_json)
}

#' format_all_episodes
#'
#' @param all_episodes 
#'
#' @return a data frame that has been cleaned cleaned and filtered of missing values
#' @export
#'
#' @examples format_all_episodes(all_episodes)
format_all_episodes <- function(all_episodes) {
  number <- lapply(all_episodes$number, replace_na)
  rating <- lapply(all_episodes$rating, replace_na)
  name <- lapply(all_episodes$name, replace_na)
  
  ep_number <- paste0("S", all_episodes$season, "E", number)
  df <- data.frame('Episode' = ep_number, 'Name' = all_episodes$name, 'Rating' = rating)
  new_col_names <- c("Episode", "Name", "Rating")
  
  colnames(df) <- new_col_names
  return(df)
}

#' generate_ratings_plot
#'
#' @param all_episodes_df 
#'
#' @return a line plot representing the average rating across the seasons of the show
#' @export
#'
#' @examples generate_ratings_plot(all_episodes_df)
generate_ratings_plot <- function(all_episodes_df) {
  all_episodes_df$Rating <- as.numeric(all_episodes_df$Rating)
  all_episodes_df$Season <- as.numeric(gsub("^S(\\d+)E.*", "\\1", all_episodes_df$Episode))
  season_avg <- aggregate(all_episodes_df$Rating, by = list(all_episodes_df$Season), FUN = mean)
  colnames(season_avg) <- c("Season", "Mean_Rating")
  base <- ggplot(season_avg, aes(x = Season, y = Mean_Rating))
  plot <- base + geom_point() + geom_line() + labs(x = "Season", y = "Mean Rating") + ggtitle("Mean Average Ratings by Season")
  
  return(plot)
}

#' generate_season_ratings_plot
#'
#' @param season_df 
#'
#' @return a plot of the rating for each episode in a given season
#' @export
#'
#' @examples generate_season_ratings_plot(season_df)
generate_season_ratings_plot <- function(season_df) {
  season_df$Rating <- as.numeric(season_df$Rating)
  season_plot <- ggplot(season_df, aes(x = Episode, y = Rating)) +
    geom_line() +
    geom_point() +
    labs(title = "Episodes Ratings",
         x = "Season Episodes",
         y = "Ratings")
  
  return(season_plot)
  
}

main <- function(){
  exit <- FALSE
  while (exit == FALSE) {
    query <- readline("Enter a Show (or 0 to exit): ")
    if (query == "0") {
      break
    }
    results <- get_shows(query)
    
    if(is.null(results)) { # if there are no results for the keywords inputted by the user
      cat("No results found")
    } else {
      # all the results related to the keywords inputted
      cat("Here are the results:\n")
      details <- format_show_name(results)
      print(details)
      
      # ask the user to select a specific show they want to view the seasons for
      seasons_input <- readline("Select a Show Number (or 0 to exit): ")
      if (seasons_input == "0") {
        break
      }
      index_seasons <- as.numeric(seasons_input)
      seasons_id <- results$id[index_seasons]
      seasons_id <- trimws(seasons_id) # to account for white space in the ID
      
      seasons <- get_seasons(seasons_id)
      season_names <- format_season_name(seasons)
      print(season_names)
      
      # ask the user to select a specific season from the show and print all of the episodes in the season
      season_number_input <- as.numeric(readline("Select a Season Number (or 0 to exit): "))
      if (season_number_input == "0") {
        break
      }
      
      season_id <- trimws(seasons$id[season_number_input])
      episodes <- get_episodes_of_season(season_id)
      
      if (is.character(episodes)) { # if there is no information about the season
        cat(episodes, "\n")
      } else { # if a data frame is returned from the get_episodes_of_season function
        episode_details <- format_episode_name(episodes)
        print(episode_details)
      }
      
      # generating plots
      cat("\n1. Plot of average rating per season for all the seasons in a show \n2. Plot of ratings for each episodes in a season")
      average_seasons_ratings <- readline("Do you want to see any of the 2 visualizations? (or 0 to exit): ")
      average_seasons_ratings
      if (average_seasons_ratings == "0") {
        break
        
        # graph for the average rating across the seasons
      } else if (average_seasons_ratings == "1") {
        all_episodes <- get_all_episodes(seasons_id)
        all_episodes_df <- format_all_episodes(all_episodes)
        plot <- generate_ratings_plot(all_episodes_df)
        print(plot)
        
        # graph for the rating for each individual episode in a season 
      } else if (average_seasons_ratings == "2") {
        season_rating_input <- as.numeric(readline("Which season do you want to visualize? "))
        season_rating_id <- trimws(seasons$id[season_rating_input])
        episodes_rating <- get_episodes_of_season(season_rating_id)
        episode_details <- format_episode_name(episodes_rating)
        episode_details$Episode <- as.numeric(str_replace(episode_details$Episode, ".*E", ""))
        season_plot <- generate_season_ratings_plot(episode_details)
        print(season_plot)
      }
    }
  }
}

main()
