\name{MessageQueue-class}
\Rdversion{1.1}
\docType{class}
\alias{MessageQueue-class}

\title{Class \code{"MessageQueue"}}
\description{

  A message queue is an ordered collection of
  \code{\linkS4class{P4Message}} objects.  The principle idea is that
  the \code{\link{fetchNextMessage}()} function will fetch the next
  unprocessed message, and consequently, this can be used to schedule
  the work for a scoring engine.

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}.

}
\details{

  The general queue functions are determined by two generic functions:
  \code{\link{fetchNextMessage}()}, and \code{\link{markAsProcessed}()}.
  The \code{\link{fetchNextMessage}()} returns the \dQuote{first} (the
  meaning of first is defined by the implementing Queue object)
  unprocssed message (i.e., \code{\link{processed}(mess)=FALSE}).
  Note that the \code{\link{fetchNextMessage}()} function will continue
  to return the same message until it is marked as processed using
  \code{\link{markAsProcessed}(queue,mess)}.  Note that simply setting
  \code{processed(mess) <- FALSE} is not sufficient because the change
  is not stored in the queue.

}
\author{Russell Almond}
\note{

  The current implementations are \code{\linkS4class{MongoQueue}} which uses a
  database collection for the queue, and a partially implemented
  \code{\linkS4class{ListQueue}} which just uses an array of messages.  This is
  not fully implemented.

  Some other alternatives would be to link to a formal queuing system,
  like Kafka, or to some kind of RPC server.

}
\seealso{
  \code{\linkS4class{MongoQueue}},
  \code{\linkS4class{ListQueue}},
  \code{\linkS4class{P4Message}},
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
showClass("MessageQueue")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} giving the name
      of the application. }
  }
}
\section{GenericFunctions}{

The following generic functions are designed to work with subclasses of
message queues, however, currently only the
\code{\linkS4class{MongoQueue}} has all of the methods.

  \describe{
    \item{\link{cleanMessageQueue}:}{
      Removes messages matching query from queue. }
    \item{\link{fetchNextMessage}:}{
      Returns the next unprocessed message from the Queue, or `NULL` if
    there are no processed messages in the queue.}
    \item{\link{importMessages}:}{
      Imports messages into a queue from a file. }
    \item{\link{markAsError}:}{
      Marks a message as an error, and saves error message in
      queue.}
    \item{\link{markAsProcessed}:}{ Marks a message as processed. }
    \item{\link{resetProcessedMessages}:}{Clears the processed flag for messages matching
      the query. }
  }
}
\section{Class-Based Methods }{
  \describe{
    \item{\code{$initialize(app, ...)}:}{ Constructor. }
  }
}
