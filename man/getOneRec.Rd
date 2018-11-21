\name{getOneRec}
\alias{getOneRec}
\alias{getManyRecs}
\title{Fetches Messages from a Mongo databas}
\description{

  This function fetches \code{\linkS4class{P4Message}} objects from a
  \code{\link[mongolite]{mongo}} database.  The message parser is passed
  as an argument, allowing it to fetch other kinds of objects than
  P4Messages.  The function \code{getManyRecs} retrieves all matching
  objects and the function \code{getOneRec} retrieves the first matching
  object. 

}
\usage{
getOneRec(jquery, col, parser, sort = c(timestamp = -1))
getManyRecs(jquery, col, parser, sort = c(timestamp = 1), limit=0)
}
\arguments{
  \item{jquery}{A string providing a Mongo JQuery to select the
    appropriate records.  See \code{\link{buildJQuery}}.
  }
  \item{col}{A \code{\link[mongolite]{mongo}} collection object to be
    queried.
  }
  \item{parser}{A function which will take the list of fields returned
    from the database and build an appropriate R object.  See
    \code{\link{parseMessage}}. 
  }
  \item{sort}{A named numeric vector giving sorting instructions.  The
    names should correpond to fields of the objects, and the values
    should be positive or negative one for increasing or decreasing
    order. Use the value \code{NULL} to leave the results unsorted.
  }
  \item{limit}{A numeric scalar giving the maximum number of objects to
    retrieve.  If 0, then all objects matching the query will be
    retrieved.}
}
\details{

  This function assumes that a number of objects (usually, but not
  necessarily subclasses of \code{\link{P4Message}} objects) have been
  stored in a Mongo database.  The \code{col} argument is the
  \code{\link[mongolite]{mongo}} object in which they are stored.  These
  functions retrive the selected objects.

  The first argument should be a string containing a JSON query
  document.  Normally, thes are constructed through a call to
  \code{\link{buildJQuery}}.  

  The query is used to create an iterator over JSON documents stored in
  the database.  At each round, the iterator extracts the JSON document
  as a (nested) list structure.  This is pased to the \code{parser}
  function to build an object of the specified type.  See the
  \code{\link{parseMessage}} function for an example parser.

  The sorting argument controls the way the returned list of objects is
  sorted. This should be a numeric vector with names giving the field
  for sorting.  The default values \code{c("timestamp"=1)} and
  \code{c("timestamp"=-1)} sort the records in ascending and decending
  order respectively.  In particular, the default value for
  \code{getOneRec} means that the most recent value will be returned.
  The defaults assume that \dQuote{timestamp} is a field of the
  stored object.  To supress sorting of outputs, use \code{NULL} as the
  argument to \code{sort}.

}
\value{

  The function \code{getOneRec} returns an object whose type is
  determined by the output of the \code{parser} function.  If
  \code{\link{parseMessage}} is used, this will be a
  \code{\linkS4class{P4Message}} object.

  The function \code{getManyRecs} returns a list of object whose type is 
  determined by the output of the \code{parser} function. 
  
}
\references{
  The MongoDB 4.0 Manual:  \url{https://docs.mongodb.com/manual/}
}
\author{Russell Almond}
\seealso{
  \code{\link{saveRec}}, \code{\link{parseMessage}},
  \code{\link{getOneRec}}, \code{\link{getManyRecs}}
  \code{\link[mongolite]{mongo}}

}
\examples{

\dontrun{
## Requires Mongo test database to be set up.

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
## collection = Messages
## Execute in Mongo Shell
## db.createUser({
## ... user: "test",
## ... pwd: "secret",
## ... roles: [{role: "readWrite", db: "test"}]
## ... });



m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)

m1@data$time <- list(tim=25.4,units="secs")
m1 <- saveRec(m1,testcol)

## Note use of oid keyword to fetch object by Mongo ID.
m1a <- getOneRec(buildJQuery("_id"=c(oid=m1@"_id")),testcol,parseMessage)
stopifnot(all.equal(m1,m1a))

m123 <- getManyRecs(buildJQuery(uid="Fred"),testcol,parseMessage)
m23 <- getManyRecs(buildJQuery(uid="Fred",sender=c("EI","EA")),
                   testcol,parseMessage)
m321 <- getManyRecs(buildJQuery(uid="Fred",timestamp=c(lte=Sys.time())),
            testcol,parseMessage,sort=c(timestamp=-1))
getManyRecs(buildJQuery(uid="Fred",
                        timestamp=c(gte=Sys.time()-as.difftime(1,units="hours"))),
                        testcol,parseMessage)
}

}
\keyword{ interface }
\keyword{ database }
