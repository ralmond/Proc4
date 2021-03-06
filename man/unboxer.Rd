\name{unboxer}
\alias{unboxer}
\alias{ununboxer}
\title{Marks scalar objects to be preserved when converting to JSON}
\description{

  The function \code{\link[jsonlite]{toJSON}} coverts vectors (which all
  R objects are) to vectors in the JSON code.  The function
  \code{jsonlite::\link[jsonlite]{unbox}} protects the object from this
  behavior, which makes the fields eaiser to search and protects against
  loss of name attributes.  The function \code{unboxer} extents
  \code{unbox} to recursively unbox lists (which preserves names).
  The function \code{ununbox} removes the unboxing flag and is mainly
  used for testing parser code.
}
\usage{
unboxer(x)
ununboxer(x)
}

\arguments{
  \item{x}{Object to be boxed/unboxed.}
}
\details{

  The  \code{jsonlite::\link[jsonlite]{unbox}} function does not
  necessarily preserve the name attributes of elements of the list.
  In other words the sequence \code{\link{as.jlist}} ->
  \code{\link[jsonlite]{toJSON}} -> \code{\link[jsonlite]{fromJSON}} ->
  \code{\link{parseMessage}} might not be the identity.

  The solution is to recursively apply \code{\link[jsonlite]{unbox}} to
  the elements of the list.  The function \code{unboxer} can be thought
  of as a recursive version of \code{unbox} which handles the entire
  tree struction.  If \code{x} is not a list, then \code{unboxer} and
  \code{unbox} are equivalent.

  The typical use of this function is defining methods for the
  \code{\link{as.jlist}} function.  This gives the implementer fine
  control of which attributes of a class should be scalars and vectors. 

  The function \code{ununbox} clears the unboxing flag.  Its main
  purpose is to be able to test various parsers.
  
}
\value{
  The function \code{unboxer} returns the object with the added class
  \code{scalar}, which is the \code{jsonlite} marker for a scalar.

  The function \code{ununboxer} returns the object without the
  \code{scalar} class marker.
}
\author{Russell Almond}
\note{

  There is a bug in the way that \code{\link[base]{POSIXt}} classes are
  handled, \code{unboxer} fixes that problem.

}
\section{Warning:  Dependence on jsonlite implementation}{

  These functions currently rely on some internal mechanisms of the
  jsonline pacakge.  In particular, it uses the internal function
  \code{jsonlite:::as.scalar}, and \code{ununbox} relies on the
  \dQuote{scalar} class mechanism.
}
\seealso{
  \code{\link[jsonlite]{unbox}}, \code{\link[jsonlite]{toJSON}},
  \code{\link{as.jlist}}, \code{\link{parseMessage}}
  
}
\examples{

## as.jlist method shows typical use of unboxer.
getMethod("as.jlist",c("P4Message","list"))

## Use ununboxer to test as.jlist/parseMessage pair.
m4 <- P4Message("Phred","Task1","PP","New Stats",
                details=list("agents"=c("ramp","ramp","lever")))
m4jl <- as.jlist(m4,attributes(m4))
m4a <- parseMessage(ununboxer(m4jl))
stopifnot(all.equal(m4,m4a))


}
\keyword{ interface }

