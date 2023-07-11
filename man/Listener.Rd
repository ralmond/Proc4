\name{Listener}
\alias{RefListener-class}
\alias{RefListener}
\alias{Listener}
\alias{Listener-class}
\alias{receiveMessage}
\alias{receiveMessage,RefListener-method}
\alias{clearMessages}
\alias{clearMessages,RefListener-method}
\alias{isListener}
\alias{isListener,ANY-method}
\alias{isListener,RefListener-method}
\alias{listenerName}
\alias{listenerName,RefListener-method}
\alias{listeningFor}
\alias{listeningFor,RefListener-method}
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
receiveMessage(x, message)
isListener(x)
\S4method{isListener}{ANY}(x)
clearMessages(x, app)
listenerName(x)
listeningFor(x, newSet)
}
\arguments{
  \item{x}{A object of the virtual class \code{Listner}.}
  \item{message}{A \code{\linkS4class{P4Message}} which is being
    transmitted.}
  \item{app}{A character scalar identifying the application served by
    the listener.}
  \item{newSet}{A character vector giving the messages the listener is
    listening for.  If empty, the listener processes all messages it
    recieves.}
}
\details{

  The \code{RefListener} class is an abstract class.  Any object can become a
  listener by giving it a method for \code{receiveMessage}.  The message
  is intended to be a subclass of \code{\linkS4class{P4Message}}, but in
  practice, no restriction is placed on the type of the message.

  As \code{RefListener} an abstract class, it means
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
\section{Fields}{

\describe{
  \item{\code{name}}{An identifier for the listener, mainly used in
  error messages.}
  \item{\code{messSet}}{A character vector giving the messages list
  listener will process.  Messages whose \code{\link{mess}} field are
  not in the list are not processed.  As a special case, if
  \code{messSet} has length 0, then all messages are processed.}
  \item{\code{db}}{An object of class \code{\link[mongo]{MongoDB}},
    which contains a database to contain the messages.  Note:  if the
    subclass does not use this, then the connection to the database will
    not be made.}
  }
}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "RefListener")}: Returns true,
      as subclasses of \code{RefListener} follow the listener protocol.}
    \item{receiveMessage}{\code{signature(x =
  	  "RefListener",message="P4Message")}: This first checks to see if
      \code{\link{mess}{(message)}} is in the \code{messList} field.  If
      so, it delegates the processing of the message to the
      \code{$recieveMessage()} method. This class-based method must be
      implemented in subclasses.}
    \item{clearMessages}{\code{signature(sender = "RefListener",
    	app="character")}: This delegates the process of cleaning the
      message collection to the \code{$reset()} class method.}
    \item{listenerName}{\code{signature(x="RefListener")}:  Returns the
    	name of the listener.}
    \item{listeningFor}{\code{signature(x="RefListener",
    	newSet="chracter")}:
      Returns the names of the messages this listener is listening for.
      If \code{newSet} is supplied, the message set is updated.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(name,db,messSet,...)}:}{ Provides default
      values for various fields. }
    \item{\code{messdb()}:}{ Returns the \code{\link[mongo]{MongoDB}}
      object in the \code{db} field.}
    \item{\code{receiveMessage(message)}:}{ Does the message processing.
      Note that the \code{RefListener} method returns and error, so
      \emph{subclasses must implement this}.  Note, also that the filtering of
      which messages to handle is done by the S4 method.}
    \item{\code{reset(app)}:}{ This method clears out the old messages.
      Again, \emph{subclasses must implement this method} as the
      \code{RefListener} class implementation raises an error.  The
      \code{app} argument is because several different implementations
      may store messages for more than one application in the same place.}
    \item{\code{listeningFor(newSet)}:}{This method returns, or if the
      second argument in present, sets the \code{messSet} field. }
  }
}
\value{

  The \code{isListener} function should return \code{TRUE} or
  \code{FALSE}, according to whether or not the object follows the
  listner protocol.

  The \code{listenerName} returns a character scalar with the name of
  the listener.

  The \code{receiveMessage} and \code{clearMessages} functions are typically
  invoked for side effects and it may have any return value.

  The \code{listeningFor} function returns a character vector giving the
  messages used by the listener.


}
\references{

  \url{https://en.wikipedia.org/wiki/Observer_pattern}
}
\author{Russell Almond}
\seealso{
  Implementing Classes:
  \code{\linkS4class{CaptureListener}},  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{UpsertListener}},
  \code{\linkS4class{InjectionListener}},
  \code{\linkS4class{TableListener}}

  Related Classes:
  \code{\linkS4class{ListenerSet}}, \code{\linkS4class{P4Message}}
}
\examples{

setRefClass("FileListener",fields=c(file="character"),
            contains="RefListener",
            methods=c(
                receiveMessage = function (message) {
                  cat("I (",listenerName(.self),
                      ") just got the message ",
                      mess(message),
                      file=file,append=TRUE)
                },
                reset = function(app) {
                  cat("\f",file=file,append=TRUE)
                }
            ))

myListener <- new("FileListener",name="Test",file="",
                  messSet="Scored Response",
                  db=mongo::MongoDB(noMongo=TRUE))

mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:15:25 EST"),
         list(correct=TRUE,seletion="D"))

mess2 <- P4Message("Fred","Task 2","Evidence ID","Raw Response",
         as.POSIXct("2018-11-04 21:16:45 EST"),
         list(seletion="C"))

isListener(myListener)
listenerName(myListener)
receiveMessage(myListener,mess1) ## This one is processed.
receiveMessage(myListener,mess2) ## This one is ignored.
clearMessages(myListener,"")
}
\keyword{ interface }
\keyword{ objects }
