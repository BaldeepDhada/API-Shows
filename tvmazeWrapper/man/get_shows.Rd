\name{get_shows}
\alias{get_shows}
\title{Get TV Show Information}
\usage{
get_shows(query)
}
\arguments{
  \item{query}{a character string specifying the query for TV show information}
}
\description{
This function gets all the TV shows matching the query.
}
\details{
An API call is made to the TVmaze database with the television show the user inputed as a parameter. The function returns a dataframe of all television shows similar to what the user inputed, as well as all associated data found within the database. NULL is returned if the television show is not found within the database.
}
\value{
Returns a dataframe containing all information on the given television show. If no matching shows are found, it returns \code{NULL}.
}
\examples{
get_shows("Hello Kitty")
}
\seealso{
\code{\link{GET}}, \code{\link{content}}, \code{\link{fromJSON}}
}
\keyword{data}
