\name{app}
\alias{app}
\alias{app<-}
\title{The application (test) a message is associated with.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
app(x)
app(x) <- val
}
\arguments{
  \item{x}{
  A message object.
}
  \item{val}{
  A string giving the new application value.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A string giving the application value.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Russell Almond}
\seealso{
  \code{\link{P4Message}}
}
\examples{
  mess <- P4Message(uid="Fred", context="Task 1",
                  sender="EIP", mess="New Observation",
                  timestamp=Sys.time(), details=list(correct=TRUE),
                  app="ecd://epls.coe.fsu.edu/Test")
stopifnot(app(mess) == "ecd://epls.coe.fsu.edu/Test")
  

}
\keyword{ manip }

