\name{Listener}
\alias{Listener-class}
\alias{receiveMessage}
\alias{isListener}
\alias{isListener,ANY-method}
\title{A listener is an object which can recieve a message.}
\description{

  A \emph{listener} an an object that takes on the observer or listerner
  role in the the listener (or observer) design pattern.  A listener
  will register itself with a speaker, and when the speaker sends a
  message it will act accordingly.  The \code{receiveMessage} generic
  function must be implemented by a listener.  It is called when the
  speaker wants to send a message.
}
\usage{
receiveMessage(x, mess)
isListener(x)
\S4method{isListener}{ANY}(x)
}
\arguments{
  \item{x}{A object of the virtual class \code{Listner}.}
  \item{mess}{A \code{\linkS4class{P4Message}} which is being transmitted.}
}
\details{

  The \code{Listener} class is a virtual class.  Any object can become a
  listener by giving it a method for \code{receiveMessage}.  The message
  is intended to be a subclass of \code{\linkS4class{P4Message}}, but in
  practice, no restriction is placed on the type of the message.

  As \code{Listener} is a virtual class, it does not have a formal
  definition.  Instead the generic function \code{isListner} is used to
  test if the object is a proper listener or not.  The default method
  checks for the presence of a \code{receiveMessage} method.  As this
  might not work properly with S3 objects, an object can also register
  itself directly by setting a method for \code{isListner} which returns
  true.

  Typically, a lister will register itself with the speaker objects.
  For example the \code{\linkS4class{ListenerSet}$addListener} method adds
  itself to a list of listeners maintained by the object.  When the
  \code{\linkS4class{ListenerSet}$notifyListeners} method is called, the
  \code{receiveMessage} method is called on each listener in the list.

}
\value{

  The \code{isListener} function should return \code{TRUE} or
  \code{FALSE}, according to whether or not the object follows the
  listner protocol.

  The \code{receiveMessage} function is typically invoked for side
  effects and it may have any return value.

}
\references{

  \url{https://en.wikipedia.org/wiki/Observer_pattern}
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{ListenerSet}}, \code{\linkS4class{P4Message}}
}
\examples{

\dontrun{## Requires Mongo database set up.
MyListener <- setClass("MyListener",slots=c("name"="character"))
setMethod("receiveMessage","MyListener",
   function(x,mess)
      cat("I (",x@name,") just got the message ",mess(mess),"\n"))


lset <-
ListenerSet$new(sender="Other",dburi="mongodb://localhost",
                colname="messages")
lset$addListener("me",MyListener())

mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:15:25 EST"),
         list(correct=TRUE,seletion="D"))

mess2 <- P4Message("Fred","Task 2","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:17:25 EST"),
         list(correct=FALSE,seletion="D"))

lset$notifyListeners(mess1)

lset$removeListener("me")

notifyListeners(lset,mess2)

}
}
\keyword{ interface }
\keyword{ objects }
