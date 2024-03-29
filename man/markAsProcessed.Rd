\name{markAsProcessed}
\alias{processed}
\alias{processingError}
\alias{markAsProcessed}
\alias{markAsProcessed,JSONDB,P4Message-method}
\alias{markAsProcessed,ListQueue-method}
\alias{markAsProcessed,MongoQueue,ANY-method}
\alias{markAsProcessed,NULL,P4Message-method}
\alias{markAsError}
\alias{markAsError,JSONDB,P4Message-method}
\alias{markAsError,ListQueue-method}
\alias{markAsError,MongoQueue,ANY-method}
\alias{markAsError,NULL,P4Message-method}
\title{Functions for manipulating entries in a message queue.}
\description{

  A collection of message objects can serve as a queue:  they can be
  sorted by their \code{\link{timestamp}} and then processed one at a
  time.  The function \code{markAsProcessed} sets the processed flag on
  the message and then saves it back to the database.  The function
  \code{processed} returns the processed flag.

  The function \code{markAsError} attaches an error to the message and
  saves it.  The function \code{processingError} returns the error (if
  it exists).

}
\usage{
markAsProcessed(col, mess)
\S4method{markAsProcessed}{JSONDB,P4Message}(col, mess)
\S4method{markAsProcessed}{ListQueue}(col, mess)
\S4method{markAsProcessed}{MongoQueue,ANY}(col, mess)
\S4method{markAsProcessed}{NULL,P4Message}(col, mess)
markAsError(col, mess, e)
\S4method{markAsError}{JSONDB,P4Message}(col, mess, e)
\S4method{markAsError}{ListQueue}(col, mess, e)
\S4method{markAsError}{MongoQueue,ANY}(col, mess, e)
\S4method{markAsError}{NULL,P4Message}(col, mess, e)
processed(x)
processingError(x)
}
\arguments{
  \item{mess}{An object of class \code{\linkS4class{P4Message}} to be
    modified. }
  \item{col}{A \code{\link[mongo]{MongoDB}} collection where the
    message queue is stored (or an object which wraps sucha collection).
    This can also be \code{NULL} in which case
    the message will not be saved to the database.
  }
  \item{e}{An object indicating the error occurred.  Note this could be
    either a string giving the error message of an object of an error
    class.  In either case, it is converted to a string before saving.
  }
  \item{x}{A message object to be queried.
  }

}
\details{

  A \code{\link[mongo]{MongoDB}} collection of messages can serve as a
  queue (see \code{\linkS4class{MongoQueue}}).
  As messages are added into the queue, the \code{processed}
  flag is set to false.  The handler then fetches them one at a time
  (sorting by the timestamp).  It then does whatever action is required
  to handle the message.  Then the function \code{markAsProcessed} is
  called to set the \code{processed} flag to true and update the entry
  in the database.

    Some thought needs to be given as to how to handle errors.  The
  function \code{markAsError} attaches an error object to the message
  and then updates it in the collection.  The error object is turned
  into a string (using \code{\link[base]{toString}}) before saving, so
  it can be any type of R object (in particular, it could be either the
  error message or the actual error object thrown by the function).

}
\value{

  The functions \code{markAsProcessed} and \code{markAsError} both
  return the modified message.

  The function \code{processed} returns a logical value indicating
  whether or not the message has been processed.

  The function \code{processingError} returns the error object attached
  to the message, or \code{NULL} if no error object is returned.  Note
  that the error object could be of any type.

}
\author{Russell Almond}
\note{

  The functions \code{markAsProcessed} and \code{markAsError} do not
  save the complete record, they just update the processed or error
  field.

  There was a bug in early version of this function, which caused the
  error to be put into a list when it was saved.  This needs to be
  carefully checked.
}
\seealso{

  \code{\linkS4class{P4Message}}, \code{\link{getOneRec}},
  \code{\link{buildJQuery}}, \code{\link{timestamp}}, \code{\linkS4class{MessageQueue}},
  \code{\link{resetProcessedMessages}}

}
\examples{

\dontrun{
col <- mongolite::mongo("TestMessages")
col$remove('{}')             # Clear out anything else in queue.
mess1 <- P4Message("One","Adder","Tester","Add me",app="adder",
                   details=list(x=1,y=1))
mess2 <- P4Message("Two","Adder","Tester","Add me",app="adder",
                   details=list(x="two",y=2))
mess1 <- saveRec(mess1,col,FALSE)
mess2 <- saveRec(mess2,col,FALSE)

mess <- getOneRec(buildJQuery(app="adder", processed=FALSE),
    col, parseMessage, sort = c(timestamp = 1))
iterations <- 0
while (!is.null(mess)) {
  if (iterations > 4L)
    stop ("Test not terminating, flag not being set?")
  iterations <- iterations + 1
  print(mess)
  print(details(mess))
  out <- try(print(details(mess)$x+details(mess)$y))
  if (is(out,'try-error'))
   mess <- markAsError(mess,col,out)
  mess <- markAsProcessed(mess,col)
  mess <- getOneRec(buildJQuery(app="adder", processed=FALSE),
    col, parseMessage, sort = c(timestamp = 1))

}

mess1a <- getOneRec(buildJQuery(app="adder",uid="One"),col,parseMessage)
mess2a <- getOneRec(buildJQuery(app="adder",uid="Two"),col,parseMessage)
stopifnot(processed(mess1a),processed(mess2a),
          is.null(processingError(mess1a)),
          grepl("Error",processingError(mess2a)))
}
}
\keyword{ interface }
\keyword{ database }
\concept{ Message-Queue}
