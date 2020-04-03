\name{MongoDB-class}
\Rdversion{1.1}
\docType{class}
\alias{MongoDB-class}
\alias{MongoDB}

\title{Class \code{"MongoDB"}}
\description{

  An S4-style class for the \code{\link[mongolite]{mongo}} class.  Note
  that this is actually a class union, allowing for \code{NULL} if the
  database is not yet initialized.  
  
}
\section{Objects from the Class}{

  \code{NULL} is an object of this class.

  Objects of this class can be created with calls to
  \code{\link[mongolite]{mongo}}. 

}
\section{Methods}{
No methods defined with class "MongoDB" in the signature.
}
\author{Russell Almond}
\note{

  The original \code{\link[mongolite]{mongo}} class is an S3 class.
  Rather than just call \code{\link[methods]{setOldClass}} and exposing
  that, I've explosed a class union
  (\code{\link[methods]{setClassUnion}}) with the \code{mongo} class and
  \code{NULL}.

  A typical usage would have this type used in the slot of an object,
  which would initialize the value to \code{NULL}, and then set it to a
  \code{mongo} object when the database connection is openned.

}
\seealso{
  \code{\link{ListenerSet}}, \code{\link[mongolite]{mongo}}
}
\examples{
showClass("MongoDB")
showClass("ListenerSet")
lset <- ListenerSet$new()
lset$messdb
}
\keyword{classes}
