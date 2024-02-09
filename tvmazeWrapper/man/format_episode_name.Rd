\name{format_episode_name}
\alias{format_episode_name}
\title{Format TV Show Episode Information}
\usage{
format_episode_name(episode)
}
\arguments{
   \item{episode}{a dataframe containing information about TV show episodes}
}
\description{
This function formats the dataframe returned in the get_all_episodes() function, returning only episode numbers, episode names, and episode ratings.
}
\value{
Returns a dataframe containing all relevant information on the episodes of a given season in a structured dataframe. Null and empty values are replaced by '?'
}
\examples{
format_episode_name(episode)
}
\seealso{
\code{\link{get_episodes_of_season}}
}
\keyword{data}
