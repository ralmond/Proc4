\name{MongoQueue-class}
\Rdversion{1.1}
\docType{class}
\alias{MongoQueue-class}
\title{Class \code{"MongoQueue"}}
\description{

  This is a message queue implemented as a database collection.

  This wraps a collection in a Mongo (or other JSON-based) database.
  The \code{\link{fetchNextMessage}} looks for the first (earliest
  timestamp) message which is not processed.

}
\section{Extends}{
Class \code{"\linkS4class{MessageQueue}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{\link{cleanMessageQueue}}{\code{signature(queue = "MongoQueue")}:
      Removes messages matching query from queue. }
    \item{\link{fetchNextMessage}}{\code{signature(queue = "MessageQueue")}:
      Returns the next unprocessed message from the Queue, or `NULL` if
      there are no processed messages in the queue.}
    \item{\link{importMessages}}{\code{signature(queue = "MongoQueue")}:
      Imports messages into a queue from a file. }
    \item{\link{markAsError}}{\code{signature(col = "MongoQueue", mess =
	"ANY")}: Marks a message as an error, and saves error message in
      queue.}
    \item{\link{markAsProcessed}}{\code{signature(col = "MongoQueue", mess =
	"ANY")}: Marks a message as processed. }
    \item{\link{resetProcessedMessages}}{\code{signature(queue =
	"MongoQueue")}: Clears the processed flag for messages matching
      the query. }
  }
}
\author{Russell Almond}
\note{

  It is probably a good idea to build an index on this database using
  the \dQuote{processed} and \dQuote{timestamp} fields.  The
  \code{$buildIndex()} method does this.

}
\seealso{
  \code{\linkS4class{MessageQueue}},\code{\linkS4class{P4Message}},
  \code{\link[mongo:MongoDB-class]{MongoDB}},
  \code{\link{fetchNextMessage}()},
  \code{\link{cleanMessageQueue}()},
  \code{\link{importMessages}()},
  \code{\link{markAsProcessed}()},
  \code{\link{resetProcessedMessages}()},
  \code{\link{buildMessage}()},
  \code{\link[mongo]{getOneRec}()}


}
\examples{
showClass("MongoQueue")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} giving the
      identifier of the application.  This is used to restrict the
      \code{\link{app}} field of the message to match the current
      application.}
    \item{\code{messDB}:}{Object of class
      \code{\link[mongo:JSONDB-class]{JSONDB}} that provides a
      reference to the database collection storing the messages.}
    \item{\code{builder}:}{Object of class \code{function} which is used
    to reconstruct the messages from the data, see
    \code{\link{buildMessage}()} and \code{\link[mongo]{getOneRec}()}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{$queue()}:}{Returns the \code{JSONDB} reference.}
    \item{\code{$initialize(app, messDB, builder, ...)}:}{ Constructor. }
    \item{\code{$fetchNextMessage()}:}{Internal implementation of the
      fetch method.}
    \item{\code{$buildIndex()}:}{This method builds an index in the
      collection.  Generally only needs to be done once.}
    \item{\code{$count()}:}{Returns the number of unprocessed messages remaining in queue.}

  }

}
