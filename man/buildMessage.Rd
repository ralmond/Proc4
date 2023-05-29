\name{buildMessage}
\alias{buildMessage}
\alias{cleanMessageJlist}
\alias{as.jlist,P4Message,list-method}
\alias{parse.jlist,P4Message,list-method}
\title{Converts a JSON object into a P4 Message}
\description{

  The \code{buildMessage} function is a parser to use with the
  \code{\link[mongo]{getOneRec}} and \code{\link[mongo]{getManyRecs}} database query
  functions.  This function will convert the documents fetched from the
  database into \code{\linkS4class{P4Message}} objects.

}
\usage{
buildMessage(rec,class="P4Message")
cleanMessageJlist(rec)
\S4method{as.jlist}{P4Message,list}(obj, ml, serialize = TRUE)
\S4method{parse.jlist}{P4Message,list}(class, rec)
}
\arguments{
  \item{rec}{A named list containing JSON data.}
  \item{class}{The class of the object being built.  In the case of
    \code{buildMessage}, this can be the name of the class.}
  \item{obj}{The object being converted.  This is mostly used for
    message dispatch.}
  \item{ml}{A named list containing JSON data.}
  \item{serialize}{If true, then the
    \code{\link[jsonlite]{serializeJSON}} method is used to preserve the
    \code{\link{details}} field of the message.}
}
\details{

  The \code{\link[mongo]{mdbIterate}} method object returns a list
  containing the fields of the JSON object with a
  \emph{name}=\emph{value} format (see \link[mongo]{jlist}).  This is
  the \code{rec} argument to \code{buildMessage}.  In particular,
  this is a builder function (see \code{\link[mongo]{buildObject}})
  which can be passed as the \code{builder} argument to
  \code{\link[mongo]{getOneRec}()} or \code{\link[mongo]{getOneRec}()}
  when the object to be built is a \code{\linkS4class{P4Message}}
  object.

  To facilitate the building subclasses, the (e.g., to check the
  argument types and insert default values).  The function
  \code{cleanMessageJlist} does that cleaning for the common fields of
  the \code{\linkS4class{P4Message}} object, so subclasses
  \code{P4Message} can inheret the parsing for the commond message
  fields.  The \code{\link[mongo]{as.jlist}} method is a helper function
  for the \code{\link[mongo]{as.json}} method.  The
  \code{\link[mongo]{parse.jlist}} method (which calls
  \code{cleanMessageJList} is a helper function for the
  \code{\link[mongo]{parse.json}} method.

  The \code{data} field needs extra care as it could contain arbitrary R
  objects.  There are two strategies for handling the data field.
  First, use \code{\link[jsonlite]{serializeJSON}} to turn the data
  field into a slob (string large object), and
  \code{\link[jsonlite]{unserializeJSON}} to decode it.  This strategy
  should cover most special cases, but does not result in easily edited
  JSON output.  Second, recursively apply \code{\link[mongo]{unboxer}}
  and use the function \code{\link[mongo]{parseSimpleData}} to undo the
  coding.  This results in output which should be more human readable,
  but does not handle objects (either S3 or S4).  It also may fail on
  more complex list structures.

}
\value{

  The function \code{buildMessage} returns a
  \code{\linkS4class{P4Message}} object populated with fields from the
  \code{rec} argument.  The function \code{cleanMessageJlist} and the
  \code{parse.jlist} method returns the cleaned \code{rec} argument
  (suitable for passing to the \code{P4Message} constructor).

  The function \code{as.jlist} method returns the processed \code{ml}
  object (ready to be converted to JSON).


}
\author{Russell Almond}
\note{

  I hit the barrier pretty quickly with trying to unparse the data
  manually.  In particular, it was impossible to tell the difference
  between a list of integers and a vector of integers (or any other
  storage type).  So, I went with the serialize solution.

  The downside of the serial solution is that it stores the data field
  as a slob.  This means that data values cannot be indexed.  If this
  becomes a problem, a more complex implementation may be needed.


}
\seealso{

  \code{\link[mongo]{as.jlist}}, \code{\link[mongo]{getOneRec}},
  \code{\link[mongo]{getManyRecs}}, \code{\linkS4class{P4Message}}

  \code{\link[jsonlite]{serializeJSON}},
  \code{\link[jsonlite]{unserializeJSON}}
}
\examples{

m1 <- P4Message("Fred","Task1","PP","Task Done",
                details=list("Selection"="B"))
m2 <- P4Message("Fred","Task1","EI","New Obs",
                details=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- P4Message("Fred","Task1","EA","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

ev1 <- P4Message("Phred","Level 1","PP","Task Done",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list("list"=list("one"=1,"two"=1:2),"vector"=(1:3)))


m1a <- buildMessage(mongo::ununboxer(as.jlist(m1,attributes(m1))))
m2a <- buildMessage(mongo::ununboxer(as.jlist(m2,attributes(m2))))
m3a <- buildMessage(mongo::ununboxer(as.jlist(m3,attributes(m3))))

ev1a <- buildMessage(mongo::ununboxer(as.jlist(ev1,attributes(ev1))))

}
\keyword{ interface }
\keyword{ database }
