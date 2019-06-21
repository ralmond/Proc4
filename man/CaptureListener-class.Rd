\name{CaptureListener-class}
\Rdversion{1.1}
\docType{class}
\alias{CaptureListener-class}
\alias{isListener,CaptureListener-method}
\alias{receiveMessage,CaptureListener-method}

\title{Class \code{"CaptureListener"}}
\description{

  This listener simply takes its messages and adds them to a list.  It
  is is mainly used for testing the message system.

}
\section{Extends}{

This class implements the \code{\link{Listener}} interface.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "CaptureListener")}: returns
    true.}
    \item{receiveMessage}{\code{signature(x = "CaptureListener")}: If
      the message is in the \code{messSet}, it adds the message to the
      message list.  (See details) }
  }
}
\details{
  
  This listener simply takes all messages and pushes them onto the
  \code{messages} field.  The \code{messages} field is the complete
  list of received messages, most recent to most ancient.  The method
  \code{lastMessage()} returns the most recent message.
}
\references{

  This is an example of the observer design pattern.
  \url{https://en.wikipedia.org/wiki/Observer_pattern}. 

}
\author{Russell Almond}
\seealso{
  \code{\link{Listener}}, \code{\linkS4class{P4Message}},
  \code{\link{CaptureListener}},
  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{UpsertListener}},
  \code{\linkS4class{InjectionListener}},
  \code{\link[mongolite]{mongo}}
}
\examples{

mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EABN",mess="Statistics",
                   details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))

cl <- CaptureListener()
receiveMessage(cl,mess1)
stopifnot(all.equal(mess1,cl$lastMessage()))

}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{messages}:}{Object of class \code{list} the list of
      messages in reverse chronological order.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{lastMessage()}:}{Returns the most recent message. }
    \item{\code{receiveMessage(mess)}:}{ Does the work of inserting the
      message.  See Details. }
    \item{\code{initialize(messages, ...)}:}{ Sets the default values for the fields. }
  }
}
