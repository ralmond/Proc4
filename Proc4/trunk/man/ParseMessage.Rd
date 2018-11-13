\name{parseMessage}
\alias{parseMessage}
\alias{parseData}
\title{Converts a JSON object into a P4 Message}
\description{

  The \code{parseMessage} function is a parser to use with the
  \code{\link{getOneRec}} and \code{\link{getManyRecs}} database query
  functions.  This function will convert the documents fetched from the
  database into \code{\linkS4class{P4Message}} objects.  The function
  \code{parseData} is a helper function for parsing the \code{data}
  field of the \code{P4Message} object.

}
\usage{
parseMessage(rec)
parseData(messData)
}
\arguments{
  \item{rec}{A named list containing JSON data.}
  \item{messData}{A named list containing JSON data.}
}
\details{

  The \code{$iterator()} method of the \code{\link[mongolite]{mongo}}
  object returns a list containing the fields of the JSON object with a
  \emph{name}=\emph{value} format.  This is the \code{rec} argument.
  The \code{parseMessage} function takes the fields of the JSON object
  and uses them to populate a corresponding
  \code{\linkS4class{P4Message}} object.

  The \code{parseData} function words on the data field.  In particular,
  the \code{mongo$iterator()} method does not do conversions to numeric
  or character values.  This function forces that conversion.

}
\value{

  The function \code{parseMessage} returns a
  \code{\linkS4class{P4Message}} object populated with fields from the
  \code{rec} argument.

  The function \code{parseMessage} returns a list containing the data.
  
}
\author{Russell Almond}
\note{
  I'm still not sure of how many use cases the \code{parseData} function
  covers.  It doesn't do any conversion on nested lists and it doesen't
  offer to convert non-list objects.  It may be better to use the
  \code{\link[jsonlite]{serializeJSON}} protocol from the
  \code{jsonlite} package here.  This implementation may change in
  future packages.
}
\seealso{

  \code{\link{as.jlist}}, \code{\link{getOneRec}},
  \code{\link{getManyRecs}}, \code{\linkS4class{P4Message}}

  \code{\link[mongo]{mongo}}
  \code{\link[jsonlite]{serializeJSON}}
}
\examples{

m1 <- P4Message("Fred","Task1","PP","Task Done",
                details=list("Selection"="B"))
m2 <- P4Message("Fred","Task1","EI","New Obs",
                details=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- P4Message("Fred","Task1","EA","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

m1a <- parseMessage(as.jlist(m1,attributes(m1)))
m2a <- parseMessage(as.jlist(m2,attributes(m2)))
m3a <- parseMessage(as.jlist(m3,attributes(m3)))

stopifnot(all.equal(m1,m1a),
          all.equal(m2,m2a),
          all.equal(m3,m3a))

\dontrun{ #Requires test DB setup.
testcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages

m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)

m1 <- saveRec(m1,testcol)
m1b <- getOneRec(buildJQuery("_id"=m1@"_id"),testcol,parseMessage)
m23 <- getOneRec(buildJQuery("uid"="Fred",sender=c("EI","EA")),
                 testcol,parseMessage)

}
}
\keyword{ interface }
\keyword{ database }
