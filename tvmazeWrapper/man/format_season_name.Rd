\name{format_season_name}
\alias{format_season_name}
\title{Format TV Show Season Information}
\usage{
format_season_name(season)
}
\arguments{
  \item{season}{a dataframe containing information about TV show seasons}
}
\description{
This function formats the dataframe returned in the get_seasons() function, returning only season numbers, season names, premier dates, end dates, and number of episodes.
}
\value{
Returns a dataframe containing all relevant information on the seasons of the given television show in a structured dataframe. Null and empty values are replaced by '?'
}
\examples{
format_season_name(season)
}
\seealso{
\code{\link{get_seasons}}
}
\keyword{data}
