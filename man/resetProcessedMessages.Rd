\name{resetProcessedMessages}
\alias{resetProcessedMessages}
\alias{resetProcessedMessages,MongoQueue-method}
\alias{resetProcessedMessages,ListQueue-method}
\title{Clears the processed flags on the matching messages}
\description{
  The \code{\linkS4class{MessageQueue}} class uses the processed field of the \code{\link{P4Message}} object to
  indicate which messages have been processed.  This method clears the processed flag, so that messages
  can be reprocessed.

}
\usage{
resetProcessedMessages(queue, repquery)
\S4method{resetProcessedMessages}{MongoQueue}(queue, repquery)
\S4method{resetProcessedMessages}{ListQueue}(queue, repquery)
}
\arguments{
  \item{queue}{An object of class \code{\linkS4class{MessageQueue}}}
  \item{repquery}{A list giving a mongo query (see \code{\link[mongo]{buildJQuery}}).  Only
  messages matching the query will be reprocessed.  [Currently, the \code{ListQueue} method
  ignores this argument.]}
}
\details{

  When operating on a \code{\linkS4class{MongoQueue}}, an update query is run which sets the processed
  field of the messages to \code{FALSE}.  The \code{repquery} is used to unmark a subset of messages.

  For the \code{\linkS4class{ListQueue}} method, all messages are unmarked regardless of the query.
}
\value{
  Function run for side effects, result is status information.
}
\author{Russell Almond}
\note{
  The current \code{ListQueue} implementation is pretty minimal, and will probably get updated.
}
\seealso{
  \code{\linkS4class{MessageQueue}}, \code{\link{markAsProcessed}}, \code{\link{fetchNextMessage}}
}
\examples{
  ## Writeme
}
\keyword{ database }
\concept{ Message-Queue }
