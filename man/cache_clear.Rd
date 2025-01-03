% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/000_setup.R
\name{cache_clear}
\alias{cache_clear}
\title{Clear your cached projects}
\usage{
cache_clear()
}
\value{
The file path of your RosyREDCap cache

messages confirming deleted cache
}
\description{
Included for transparency and confirmation/testing.
}
\details{
This function checks the location of the cache established by \code{\link[hoardr]{hoard}} and deletes it!
This will not delete project data, just the packages "memory" of it.
}
\examples{

\dontrun{
#warning this will delete the packages cache of project locations
cache_clear()
}
}
\seealso{
Other Project Cache Functions: 
\code{\link{cache_path}()},
\code{\link{get_projects}()},
\code{\link{project_health_check}()}
}
\concept{Project Cache Functions}
\keyword{Cache}
\keyword{Functions}
\keyword{Project}
