\name{fetchNextMessage}
\alias{fetchNextMessage}
\alias{fetchNextMessage,MessageQueue-method}
\title{Returns the next unprocessed message from a message queue.}
\description{
  Searchers through the messages for the first unprocessed message.  Return \code{NULL} if none is found.
}
\usage{
fetchNextMessage(queue)
\S4method{fetchNextMessage}{MessageQueue}(queue)
}
\arguments{
  \item{queue}{The \code{\linkS4class{MessageQueue}} to search.}
}
\details{

  The \code{ListQueue} message iterates through its internal collection
  until it finds an unprocessed message, or it runs out of messages.
  The \code{MongoQueue} message searches the collection.

}
\value{
  Either an object of class \code{\linkS4class{P4Message}} or
  \code{NULL} if there are no remaining unprocessed messages.
}
\author{Russell Almond}
\note{

  The \code{ListQueue} method returns the current message if it has not
  been processed.  Otherwise, it increments to pointer until if either
  finds an unprocessed messages or runs out of messages.

  The \code{MongoQueue} method sorts the unprocessed messages by
  timestamp, and returns the one with the earliest message.

  In both cases, \code{\link{markAsProcessed}} must be called on the
  processed message to advance the queue.

}
\seealso{
  \code{\linkS4class{MessageQueue}}, \code{\link{markAsProcessed}},
  \code{\link{resetProcessedMessages}}
}
\examples{
  messy <- list(
    P4Message("test","Test 1","Tester","Test Message"),
    P4Message("test","Test 2","Tester","Test Message",processed=TRUE),
    P4Message("test","Test 3","Tester","Test Message"))
  messq <- new("ListQueue","Qtest",messy)
  mess1 <- fetchNextMessage(messq)
  mess1
  fetchNextMessage(messq)
  markAsProcessed(messq,mess1)
  mess2 <- fetchNextMessage(messq)
  mess2
  markAsProcessed(messq,mess2)
  mess3 <- fetchNextMessage(messq)


}
\keyword{ interface }
\concept{ Message-Queue }

