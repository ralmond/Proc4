\name{as.json}
\alias{as.json}
\alias{as.json,ANY-method}
\alias{as.jlist}
\title{Converts P4 messages to JSON representation}
\description{

  These methods extend the \code{\link[jsonlite]{toJSON}} function
  providing an extensible protocol for serializing S4 objects.  The
  function \code{as.json} turns the object into a string containing a
  JSON document by first calling \code{as.jlist} to convert the object
  into a list and then calling \code{toJSON} to do the work.

}
\usage{
as.json(x)
\S4method{as.json}{ANY}(x)
as.jlist(obj,ml)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An (S4) object to be serialized.}
  \item{obj}{The object being serialized}
  \item{ml}{A list of fields of the object.}
}
\details{

  The existing \code{\link[jsonlite]{toJSON}} does not support S4
  objects, and the \code{\link[jsonlite]{serializeJSON}} provides too
  much detail; so while it is good for saving and restoring R objects,
  it is not good for sharing data between programs.  The function
  \code{as.json} and \code{as.jlist} are S4 generics, so they can be
  easily extended to other classes.

  The default method for \code{as.json} is essentially \code{toJSON(
  as.jlist(x, attributes(x)))}.  The function \code{attributes(x)} turns
  the fields of the object into a list, and then the appropriate method
  for \code{as.jlist} further processes those objects.  For example, it
  can set the \code{"_id"} field used by the Mongo DB as a unique
  identifier (or other derived fields) to \code{NULL}.

  Another important step is to call \code{unboxer} on fields which should
  not be stored as vectors.  The function \code{toJSON} by default wraps
  all R objects in \sQuote{[]} (after all, they are all vectors), but
  that is probably not useful if the field is to be used as an index.
  Wrapping the field in \code{unboxer()}, i.e., using \code{ml$field <-
  unboxer(ml$field)}, suppresses the brackets.  The function
  \code{unboxer()} in this package is an extension of the
  \code{jsonlite::\link[jsonlite]{unbox}} function, which does not
  properly unbox POSIXt objects.

  Finally, for a field that can contain arbitrary R objects, the
  function \code{\link{unparseData}} coverts the data into a JSON string
  which will completely recover the data.

}
\value{

  The function \code{as.json} returns a unicode string with a serialized
  version of the object.

  The function \code{as.jlist} returns a list of the fields of the
  object which need to be serialized (usually through a call to
  \code{\link[jsonlite]{toJSON}}.

}
\author{Russell Almond}
\seealso{
  In this package:
  \code{\link{parseMessage}}, \code{\link{saveRec}},
  \code{\link{parseData}}
  
  In the jsonlite package:
  \code{\link[jsonlite]{toJSON}}, \code{\link[jsonlite]{serializeJSON}},
  \code{jsonlite::\link[jsonlite]{unbox}}
}
\examples{
mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:15:25 EST"),
         list(correct=TRUE,seletion="D"))
as.json(mess1)

\dontrun{
## This is the method for P4 Messages.
setMethod("as.jlist",c("P4Message","list"), function(obj,ml) {
  ml$"_id" <- NULL
  ml$class <-NULL
  ## Use manual unboxing for finer control.
  ml$app <- unboxer(ml$app)
  ml$uid <- unboxer(ml$uid)
  if (!is.null(ml$context) && length(ml$context)==1L)
    ml$context <- unboxer(ml$context)
  if (!is.null(ml$sender) && length(ml$sender)==1L)
    ml$sender <- unboxer(ml$sender)
  if (!is.null(ml$mess) && length(ml$mess)==1L)
    ml$mess <- unboxer(ml$mess)
  ml$timestamp <- unboxer(ml$timestamp) # Auto_unboxer bug.
  ## Saves name data; need recursvie version.
  ml$data <- unparseData(ml$data)
  ml
  })
}

}
\keyword{ IO }
\keyword{ interfaces }
