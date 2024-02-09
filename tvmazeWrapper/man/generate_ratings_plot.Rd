\name{generate_ratings_plot}
\alias{generate_ratings_plot}
\title{Generate Ratings Plot}
\usage{
generate_ratings_plot(all_episodes)
}
\arguments{
  \item{all_episodes}{a dataframe containing information about all episodes of the TV show}
}
\description{
This function generates a line plot representing the average rating across the seasons of the TV show.
}
\details{
This function utilizes the data generated from the format_episode_name() function to produce a visualization of a television shows average rating per season plotted over season.
}
\value{
Returns a line plot representing the average rating across the seasons of the show.
}
\examples{
generate_ratings_plot(all_episodes)
}
\seealso{
\code{\link{ggplot}}, \code{\link{geom_point}}, \code{\link{geom_line}}
}
\keyword{data}
