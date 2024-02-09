\name{get_episodes_of_season}
\alias{get_episodes_of_season}
\title{Get TV Show Season Episodes Information}
\usage{
get_episodes_of_season(season_id)
}
\arguments{
   \item{season_id}{a numeric identifier specifying the ID of the TV show season}
}
}
\description{
This function retrieves information about the episodes of a television show season based on the provided season ID.
}
\details{
A third API call is made to the TVMaze database with season id as a parameter. This function returns a dataframe of all available data pertaining to the season of a television show.
}
\value{
Returns a data frame of the parsed JSON containing information about the episodes of the specified TV show season.
}
\examples{
get_episodes_of_season(season_id)
}
\seealso{
\code{\link{GET}}, \code{\link{content}}, \code{\link{fromJSON}}
}
\keyword{data}
