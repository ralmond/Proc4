\name{cleanMessageQueue}
\alias{cleanMessageQueue}
\alias{cleanMessageQueue,MongoQueue-method}
\title{Removes messages matching query from queue.}
\description{

  Often a queue will contain a number of messages which do not get processed.  This function
  cleans out messages from the queue.  This is typically called both
  before (called \dQuote{cleaning}) importing new messages (see
  \code{\link{importMessages}}) and after (called \dQuote{purging}).

}
\usage{
cleanMessageQueue(queue, query, appid)
\S4method{cleanMessageQueue}{MongoQueue}(queue, query, appid)
}
\arguments{
  \item{queue}{An object of class \code{\linkS4class{MessageQueue}} to
    be cleaned.}
  \item{query}{A list which forms a Mongo query for
    selecting the messages to be removed.  See
    \code{\link[mongo]{buildJQuery}}.}
  \item{appid}{A character scalar giving the name of the application to
    be cleaned.}
}
\value{
  Return value is undefined.  Called for its side effects (removing
  messages from database collection).

  Will log and throw database errors.
}
\author{Russell Almond}
\note{
  Generates log entries using `futile.logger`.

  Currently no method for `ListQueue` objects.
}
\seealso{
  \code{\linkS4class{MongoQueue}}
}
\examples{
  mq <- new("MongoQueue","QueueTest",mongo::MongoDB("Messages",noMongo=TRUE),
                   builder=buildMessage)
  ## Remove Fred's messages from the database.
  cleanMessageQueue(mq,list(c(uid="Fred")),"QueueTest")
  ## Purge NO-OP messages from the imported data.
  cleanMessageQueue(mq,list(c(mess="NO-OP")),"QueueTest")

}
\keyword{ database }
\concept{ Message-Queue }

