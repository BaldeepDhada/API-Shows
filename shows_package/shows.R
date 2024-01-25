library(httr)
library(jsonlite)
library(glue)
library(roxygen2)

BASE_URL = "https://api.tvmaze.com/"

get_shows <- function(query){
  query = paste0(BASE_URL, "search/shows?q=", URLencode(query))
  response = GET(query)
  json_content = content(response, "text", encoding = "UTF-8")
  parse_json = fromJSON(json_content)$show
  ifelse(length(parse_json) == 0, return (NULL), return (parse_json))
}

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

main <- function(){
  query <- readline("Search for a Show: ")
  results <- get_shows(query)
  
  if(is.null(results)) {
    cat("No results found")
  } else {
    cat("Here are the results:")
    details <- format_show_name(results)
    print(details)
    
    seasons_input <- readline("Select a Show: ")
    index_seasons <- as.numeric(seasons_input)
    season_id <- results$id[index_seasons]
    
    seasons <- get_seasons(season_id)
    season_names <- format_season_name(seasons)
    print(season_names)
  }
}

main()
