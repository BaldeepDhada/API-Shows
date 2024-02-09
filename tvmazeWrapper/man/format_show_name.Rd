\name{format_show_name}
\alias{format_show_name}
\title{Format Show Data}
\usage{
format_show_name(shows)
}
\arguments{
  \item{shows}{a dataframe containing TV show information from get_shows}
}
\description{
This function formats the dataframe returned in the get_shows() function, returning only the television show name, premier date, end date, and genres. It cleans the data of NA values.
}
\value{
Returns a dataframe containing all relevant information on the given television show in a structured dataframe. Null and empty values are replaced by '?'
}
\examples{
format_show_name(shows)
}
\seealso{
\code{\link{get_shows}}
}
\keyword{data}
