\name{notifyListeners}
\alias{notifyListeners}
\title{Notifies listeners that a new message is available.}
\description{

  This is a generic function for objects that send
  \code{\link{P4Message}} objects.  When this function is called, the
  message is sent to the listeners; that is, the
  \code{\link{receiveMessage}} function is called on the listener
  objects.  Often, this protocol is implemented by having the
  \code{sender} include a \code{\link{ListenerSet}} object.
  
}
\usage{
notifyListeners(sender, message)
}
\arguments{
  \item{sender}{An object which sends messages. }
  \item{message}{A \code{\link{P4Message}} to be sent.}
}
\value{

  Function is invoked for its side effect, so return value may be
  anything.
  
}
\author{Russell Almond}
\seealso{
  \code{\link{P4Message}}, \code{\link{Listener}},
  \code{\link{ListenerSet}} 
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
