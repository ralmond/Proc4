\name{ListenerSet-class}
\Rdversion{1.1}
\docType{class}
\alias{ListenerSet-class}
\alias{isListener,ListenerSet-method}
\alias{receiveMessage,ListenerSet-method}

\title{Class \code{"ListenerSet"}}
\description{

  This is a \dQuote{mix-in} class that adds a speaker protocol to an
  object, which is complementary to the \code{\link{Listener}}
  protocol.  This object maintains a list of listeners.  When the
  \code{notifyListeners} method is called, it notifies each of the
  listeners by calling the \code{\link{receiveMessage}} method on the
  listener. 
}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "ListenerSet")}: Returns true,
    as the ListenerSet follows the listener protocol.}
    \item{receiveMessage}{\code{signature(x = "ListenerSet")}: A synonym
    for \code{notifyListeners}.}
}
}
\section{Protocol}{

  The key to this class is the \code{notifyListeners} method.  This
  method should receive as its argument a \code{\link{P4Message}} object.
  (The protocol is fairly robust to the type of message and the
  type is not enforced. In fact, any object which has a
  \code{\link{as.jlist}} method should work.)

  When the notifier is called it performs the following functions:
  \enumerate{
    \item{It saves the message to the collection represented by
      \code{messdb}.}
    \item{It calls the \code{\link{receiveMessage}} method on each of
      the objects in the listener list.}
    \item{It logs the messages sent using the
      \code{\link[futile.logger]{flog.logger}}, in the \code{"Proc4"} logger.
      The sending of the messages is logged a the \dQuote{INFO} level,
      and the actual message at the \dQuote{DEBUG} level.}
  }

  In addition, the \code{ListenerSet} maintains a named list of
  \code{\link{Listener}} objects (that is, objects that have a
  \code{receiveMessage} method).  The methods \code{addListener} and
  \code{removeListener} maintain this list.
}
\references{
  \url{https://en.wikipedia.org/wiki/Observer_pattern}
}
\author{Russell Almond}
\note{

  The \code{notifyListeners} method uses the
  \code{\link[futile.logger]{flog.logger}} protocol.  In particular, it
  logs sending the message at the \dQuote{INFO} level, and the actual
  message sent at the \dQuote{DEBUG} level.  In particular, setting
  \code{\link[futile.logger]{flog.threshold}(DEBUG,name="Proc4")} will
  turn on logging of the actual message and
  \code{\link[futile.logger]{flog.threshold}(WARN,name="Proc4")} will
  turn off logging of the message sent messages.

  It is often useful to redirect the Proc4 logger to a log file.  In
  addition, changing the logging format to JSON, will allow the message
  to be recovered.  Thus, try
  \code{\link[futile.logger]{flog.layout}(\link[futile.logger]{layout.json},name="Proc4"}
  to activate logging in JSON format.
  
}
\seealso{

  \code{\link{Listener}}, \code{\link[futile.logger]{flog.logger}},
  \code{\link[mongo]{mongolite}}
  
}
\examples{
showClass("ListenerSet")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{sender}:}{Object of class \code{character}:the name of
      the source of the messages. }
    \item{\code{dburi}:}{Object of class \code{character}: the URI for
      the \code{\link[mongolite]{mongo}} database.}
    \item{\code{colname}:}{Object of class \code{character}: the name of
      the column in which messages should be logged.}
    \item{\code{listeners}:}{A named \code{list} of
      \code{\link{Listener}} objects, that is objects for which
      \code{\link{isListener}} is true.}
    \item{\code{messdb}:}{Object of class \code{mongo} which is a handle
      to the collection where messages are logged.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{notifyListeners(mess)}:}{This method calls
      \code{\link{receiveMessage}} on all of the listeners.  See
      Protocol section above. }
    \item{\code{addListener(name, listener)}:}{ This method addes a
      lsitener to the list.}
    \item{\code{initialize(sender, dburi, listeners, colname, ...)}:}{This
      creates the listener.  In particular, it call
      \code{\link[mongolite]{mongo}(colnam,uri=dburi)} to open the
      collection for logging.}
    \item{\code{removeListener(name)}:}{ This removes a listener from
      the collection by its name.}
  }
}
