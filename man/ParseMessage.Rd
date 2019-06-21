\name{parseMessage}
\alias{parseMessage}
\alias{parseData}
\alias{parseSimpleData}
\alias{unparseData}
\alias{cleanMessageJlist}
\title{Converts a JSON object into a P4 Message}
\description{

  The \code{parseMessage} function is a parser to use with the
  \code{\link{getOneRec}} and \code{\link{getManyRecs}} database query
  functions.  This function will convert the documents fetched from the
  database into \code{\linkS4class{P4Message}} objects.  The function
  \code{parseData} is a helper function for parsing the \code{data}
  field of the \code{P4Message} object, and \code{unparseData} is its
  inverse. 

}
\usage{
parseMessage(rec)
cleanMessageJlist(rec)
parseData(messData)
parseSimpleData(messData)
unparseData(data, serialize=TRUE)
}
\arguments{
  \item{rec}{A named list containing JSON data.}
  \item{messData}{A named list containing JSON data.}
  \item{data}{An R object to be serialized.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code.} 
}
\details{

  The \code{$iterator()} method of the \code{\link[mongolite]{mongo}}
  object returns a list containing the fields of the JSON object with a
  \emph{name}=\emph{value} format.  This is the \code{rec} argument.
  The \code{parseMessage} function takes the fields of the JSON object
  and uses them to populate a corresponding
  \code{\linkS4class{P4Message}} object.  Usually, some cleaning is done
  first (e.g., to check the argument types and insert default values).
  The function \code{cleanMessageJlist} does that cleaning for the
  common fields of the \code{P4Message} object, so
  subclasses \code{P4Message} can inheret the parsing for the commond
  message fields.


  The \code{data} field needs extra care as it could contain arbitrary R
  objects.  There are two strategies for handling the data field.
  First, use \code{\link[jsonlite]{serializeJSON}} to turn the data field
  into a slob (string large object), and
  \code{\link[jsonlite]{unserializeJSON}} to decode it.  This strategy
  should cover most special cases, but does not result in easily edited
  JSON output.  Second, recursively apply \code{\link{unboxer}} 
  and use the function \code{parseSimpleMessage} to undo the coding.
  This results in output which should be more human readable, but does
  not handle objects (either S3 or S4).  It also may fail on more
  complex list structures.

}
\value{

  The function \code{parseMessage} returns a
  \code{\linkS4class{P4Message}} object populated with fields from the
  \code{rec} argument.  The function \code{cleanMessageJlist} returns
  the cleaned \code{rec} argument.

  The function \code{unparseData} returns a JSON string representing the
  data.  The functions \code{parseData} and \code{parseSimpleData} return a
  list containing the data. 
  
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

  \code{\link{as.jlist}}, \code{\link{getOneRec}},
  \code{\link{getManyRecs}}, \code{\linkS4class{P4Message}}

  \code{\link[mongolite]{mongo}},
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


m1a <- parseMessage(ununboxer(as.jlist(m1,attributes(m1))))
m2a <- parseMessage(ununboxer(as.jlist(m2,attributes(m2))))
m3a <- parseMessage(ununboxer(as.jlist(m3,attributes(m3))))

ev1a <- parseMessage(ununboxer(as.jlist(ev1,attributes(ev1))))

stopifnot(all.equal(m1,m1a),
          all.equal(m2,m2a),
          all.equal(m3,m3a),
          all.equal(ev1,ev1a))

\dontrun{ #Requires test DB setup.
testcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages
testcol$remove('{}')  ## Clear everything for test.                  

m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)
ev1 <- saveRec(ev1,testcol)

m1 <- saveRec(m1,testcol)
m1b <- getOneRec(buildJQuery("_id"=c("oid"=m1@"_id")),testcol,parseMessage)
stopifnot(all.equal(m1,m1b))
m23 <- getManyRecs(buildJQuery("uid"="Fred",sender=c("EI","EA")),
                  testcol,parseMessage)
stopifnot(length(m23)==2L)
ev1b <- getOneRec(buildJQuery("uid"="Phred"),
                  testcol,parseMessage)
stopifnot(all.equal(ev1,ev1b))

}
}
\keyword{ interface }
\keyword{ database }
