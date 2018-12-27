\name{parseMessage}
\alias{parseMessage}
\alias{parseData}
\alias{unparseData}
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
parseData(messData)
unparseData(data)
}
\arguments{
  \item{rec}{A named list containing JSON data.}
  \item{messData}{A named list containing JSON data.}
  \item{data}{An R object to be serialized.}
}
\details{

  The \code{$iterator()} method of the \code{\link[mongolite]{mongo}}
  object returns a list containing the fields of the JSON object with a
  \emph{name}=\emph{value} format.  This is the \code{rec} argument.
  The \code{parseMessage} function takes the fields of the JSON object
  and uses them to populate a corresponding
  \code{\linkS4class{P4Message}} object.

  The data field needs special care.  Because it could contain arbitrary
  R objects, it is put through \code{\link[jsonlite]{serializeJSON}} and
  \code{\link[jsonlite]{unserializeJSON}}.
  The function \code{unparseData} is a synonym for \code{serializeJSON}
  and \code{parseData} is a synonym for \code{unserializeJSON}. 

}
\value{

  The function \code{parseMessage} returns a
  \code{\linkS4class{P4Message}} object populated with fields from the
  \code{rec} argument.

  The function \code{unparseData} returns a JSON string representing the
  data. 
  The function \code{parseData} returns a list containing the data.
  
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
testcol$remove('{}')

m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)
ev1 <- saveRec(ev1,testcol)

m1 <- saveRec(m1,testcol)
m1b <- getOneRec(buildJQuery("_id"=c("oid"=m1@"_id")),testcol,parseMessage)
stopifnot(all.equal,m1,m1b)
m23 <- getManyRecs(buildJQuery("uid"="Fred",sender=c("EI","EA")),
                  testcol,parseMessage)

ev1b <- getOneRec(buildJQuery("uid"="Phred"),
                  testcol,parseMessage)
stopifnot(all.equal(ev1,ev1b))

}
}
\keyword{ interface }
\keyword{ database }
