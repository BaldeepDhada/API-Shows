library(httr)
library(jsonlite)

BASE_URL = "https://api.tvmaze.com/"

get_shows = function(query){
  query = paste0(BASE_URL, "search/shows?q=", URLencode(query))
  response = GET(query)
  json_content = content(response, "text", encoding = "UTF-8")
  parse_json = fromJSON(json_content)$show
  ifelse(length(parse_json) == 0, return (NULL), return (parse_json))
}

format_show_name = function(show){
  
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

main = function(){
  query = readline("Search for a show : ")
  results = get_shows(query)
  
  ifelse(is.null(results), print("No results found"), n = 1)
  
  print("Here are the results:")
  format_show_name(results)
}

main()


