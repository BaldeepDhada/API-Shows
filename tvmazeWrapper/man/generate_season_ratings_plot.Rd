\name{generate_season_ratings_plot}
\alias{generate_season_ratings_plot}
\title{Generate Season Ratings Plot}
\usage{
generate_season_ratings_plot(season_df)
}
\arguments{
  \item{season_df}{a dataframe containing information about episodes in a given season}
}
\description{
This function generates a plot of the rating for each episode in a given season.
}
\details{
This function utilizes the data generated from the format_all_episodes() function to produce a visualization of a television shows average rating per episode plotted over episode.
}
\value{
Returns a plot of the rating for each episode in a given season.
}
\examples{
generate_season_ratings_plot(season_df)
}
\seealso{
\code{\link{ggplot}}, \code{\link{geom_point}}, \code{\link{geom_line}}
}
\keyword{data}
