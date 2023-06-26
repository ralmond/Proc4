\name{ListQueue-class}
\Rdversion{1.1}
\docType{class}
\alias{ListQueue-class}
\alias{markAsError,ListQueue,ANY-method}
\alias{markAsProcessed,ListQueue,ANY-method}

\title{Class \code{"ListQueue"}}
\description{

  This is a minimal implementation of the
  \code{\linkS4class{MessageQueue}} abstract class.  In this case, the
  messages are just held in an internal array.  It probably works well
  for short queues, and does not require a database or other external
  connection, so it useful for testing.

}
\section{Extends}{
Class \code{"\linkS4class{MessageQueue}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\details{

  The queue is implemented with a list and a pointer to the current
  position in the list.  The \code{$hasNext()} and \code{$nextMessage()}
  methods implement a typical iterator paradigm.

  Note that the \code{\linkS4class{MessageQueue}} paradigm is slightly
  different.  Here the current message is the one returned by
  \code{\link{fetchNextMessage}()} until is is marked as processed
  (\code{\link{markAsProcessed}()}), which will then cause the
  \code{$nextMessage()} method to be called advancing the postition.
}
\section{Methods}{
  \describe{
    \item{\link{fetchNextMessage}}{\code{signature(queue = "MessageQueue")}:
      Returns the next unprocessed message from the Queue, or `NULL` if
      there are no processed messages in the queue.}
    \item{\link{markAsError}}{\code{signature(col = "ListQueue", mess = "ANY")}: 
      Marks a message as an error, and saves error message in
      queue.}
    \item{\link{markAsProcessed}}{\code{signature(col = "ListQueue", mess = "ANY")}: 
      Marks a message as processed. }
    \item{\link{resetProcessedMessages}}{\code{signature(queue =
	"MongoQueue")}: Clears the processed flag for messages matching
      the query. }
  }

  Note, that currently there is no implementing method for
  \code{\link{cleanMessageQueue}} or \code{\link{importMessages}}.
  
}
\author{Russell Almond}
\note{

  This is an experimental implementation, and details may change in
  future release.

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
showClass("ListQueue")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} giving ID of application. }
    \item{\code{messages}:}{Object of class \code{list} giving the messages. }
    \item{\code{pos}:}{Object of class \code{integer} giving the current
    position of the queue.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{$nextMessage()}:}{Advances the position, and returns the
      next message (or `NULL` if all messages have been returned.}
    \item{\code{$getCurrent()}:}{ Returns the current message. }
    \item{\code{$initialize(app, messages, ...)}:}{ initializer }
    \item{\code{$reset()}:}{ Resets the position to the beginnnig of the
      queue.}
    \item{\code{$setCurrent(newmess)}:}{Updates the message at the
      current position.}
    \item{\code{hasNext()}:}{Returns true if there are more messages in
      the queue. }
    \item{\code{fetchNextMessage()}:}{Fetches the next unprocessed
      message.  This is either the current message, if not processed, or
      the \code{$nextMessage()} method is called until the first
      unprocessed message is found.}
  }

}
