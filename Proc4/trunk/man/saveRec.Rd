\name{saveRec}
\alias{saveRec}
\title{Saves a P4 Message object to a Mongo database}
\description{

  This function saves an S4 object as a record in a Mongo databalse.  It
  uses \code{\link{as.json}} to covert the object to a JSON string.
}
\usage{
saveRec(mess, col)
}
\arguments{
  \item{mess}{The message (object) to be saved.}
  \item{col}{A mongo collection object, produced with a call to
  \code{\link[mongolite]mong()}}.}
}
\value{

  Returns the message argument, which may be modified by setting the
  \code{"_id"} field if this is the first time saving the object.
}
\author{Russell Almond}
\seealso{

  \code{\link{as.json}}, \code{\linkS4class{P4Message}},
  \code{\link{parseMessage}}, \code{\link{getOneRec}},
  \code{\link[mongolite]{mongo}}
}
\examples{

\dontrun{## Need to set up database or code won't run.
m1 <- P4Message("Fred","Task1","PP","Task Done",
                details=list("Selection"="B"))
m2 <- P4Message("Fred","Task1","EI","New Obs",
                details=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- P4Message("Fred","Task1","EA","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

testcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages

## Save them back to capture the ID.
m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)

}
}
\keyword{ database }
