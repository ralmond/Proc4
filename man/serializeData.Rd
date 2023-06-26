\name{serializeData}
\alias{serializeData}
\title{Produces a string with a JSON representation of an R object}
\description{

  This function wraps the \code{\link[jsonlite]{serializeJSON}} with \code{\link[base]{encodeString}}
  to properly quote internal quotes.  This \emph{slob} (string large object) can be stored in a database.
  In particular, it is the default method for the \code{jsonEncoder} field of the \code{\linkS4class{UpdateListener}} object.


}
\usage{
serializeData(jlist)
}
\arguments{
  \item{jlist}{A list containing the data to be serialized.}
}
\value{
  A quoted string containing the JSON representation of the argument.
}
\author{Russell Almond}
\seealso{
  \code{\link[jsonlite]{serializeJSON}}, \code{\link[base]{encodeString}},
  \code{\linkS4class{UpdateListener}}
}
\examples{
dat <- list(response="b", score=1)
serializeData(dat)
}
\keyword{ database }
\keyword{ interface }

