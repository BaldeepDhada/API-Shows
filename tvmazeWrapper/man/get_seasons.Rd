\name{get_seasons}
\alias{get_seasons}
\title{Get Seasons Information for TV Shows}
\usage{
get_seasons(show_id)
}
\arguments{
  \item{show_id}{a numeric identifier specifying the ID of the TV show}
}
\description{
This function retrieves information about the seasons of a television show based on the provided show ID.
}
\details{
A second API call is made to the TVmaze database with television show id as a parameter. This function returns a dataframe of all available data pertaining to a specific television show.
}
\value{
Returns a dataframe containing all information on the seasons of the given television show id. If no matching shows are found, it returns \code{NULL}.
}
\examples{
get_seasons(1505)
}
\seealso{
\code{\link{GET}}, \code{\link{content}}, \code{\link{fromJSON}}
}
\keyword{data}
