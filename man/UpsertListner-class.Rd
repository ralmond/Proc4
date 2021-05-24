\name{UpsertListener-class}
\Rdversion{1.1}
\docType{class}
\alias{UpsertListener-class}
\alias{isListener,UpsertListener-method}
\alias{receiveMessage,UpsertListener-method}
\alias{listenerName,UpsertListener-method}
\title{Class \code{"UpsertListener"}}
\description{

  This listener takes messages that match its incomming set and inject
  them into another Mongo database (presumably a queue
  for another service).  If a matching message exists, it is replaced
  instead. 

}
\section{Extends}{

This class implements the \code{\link{Listener}} interface.

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "UpsertListener")}: returns
    true.}
    \item{receiveMessage}{\code{signature(x = "UpsertListener")}: If
      the message is in the \code{messSet}, it saves or replaces the
      message inthe database.  (See details) }
    \item{listenerName}{Returns the name assigned to the listener.}
  }
}
\details{

  The database is a \code{\link[mongolite]{mongo}} collection identified
  by \code{dburi}, \code{dbname} and \code{colname} (collection within
  the database).  The \code{mess} field of the \code{\link{P4Message}}
  is checked against the applicable messages in \code{messSet}.  If it
  is there, then the message is saved in the collection.

  Before the message is saved, the collection is checked to see if
  another message exits which matches on the fields listed in
  \code{qfields}.  If this is true, the message in the database is
  replaced.  If not, the message is inserted.  

}
\references{

  This is an example of the observer design pattern.
  \url{https://en.wikipedia.org/wiki/Observer_pattern}. 

}
\author{Russell Almond}
\seealso{
  \code{\link{Listener}}, \code{\linkS4class{P4Message}},
  \code{\link{UpsertListener}},
  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{CaptureListener}},
  \code{\linkS4class{InjectionListener}},
  \code{\linkS4class{TableListener}},
  \code{\link[mongolite]{mongo}}
}
\examples{
\dontrun{
mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EABN",mess="Statistics",
                   details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
ul <- UpsertListener(colname="Statistics",qfields=c("app","uid"),
         messSet=c("Statistics"))
receiveMessage(ul,mess1)
}
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{sender}:}{Object of class \code{character} which is used
      as the sender field for the message.}
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the Mongo database }
    \item{\code{dburi}:}{Object of class \code{character} giving the url
      of the Mongo database.}
    \item{\code{colname}:}{Object of class \code{character} giving the
      column of the Mongo database. }
    \item{\code{qfields}:}{Object of class \code{character} giving the
      names of the fields which should be considered a key for the
      messages.}
    \item{\code{messSet}:}{A vector of class \code{character} giving the
      name of messages which are sent to the database.  Only messages
      for which \code{mess(mess)} is an element of \code{messSet} will
      be inserted.}
    \item{\code{db}:}{Object of class \code{MongoDB} giving the
      database.  Use \code{messdb()} to access this field to makes sure
      it has been set up.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{messdb()}:}{Accessor for the database collection.
      Initializes the connection if it has not been set up.}
    \item{\code{receiveMessage(mess)}:}{ Does the work of inserting the
      message.  See Details. }
    \item{\code{reset(app)}:}{ Empties the database collection of
      messages with this app id.}
    \item{\code{initialize(sender, dbname, dburi, colname, messSet,
        qfields, ...)}:}{ Sets the default values for the fields. }
  }
}
