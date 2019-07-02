\name{ListenerConstructors}
\alias{CaptureListener}
\alias{InjectionListener}
\alias{UpdateListener}
\alias{UpsertListener}
\title{Constructors for Listener Classes}
\description{

  These functions create objects of class
  \code{\linkS4class{CaptureListener}},  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{UpsertListener}}, and
  \code{\linkS4class{InjectionListener}}. 

  
}
\usage{
CaptureListener(messages = list(), ...) 
InjectionListener(sender = "sender", dbname = "test",
    dburi = "mongodb://localhost", 
    messSet = character(), colname = "Messages", ...)
UpdateListener(dbname = "test", dburi = "mongodb://localhost",
    messSet = character(), colname = "Messages", targetField = "data",
    qfields = c("app", "uid"), jsonEncoder = "unparseData", ...)
UpsertListener(sender = "sender", dbname = "test",
    dburi = "mongodb://localhost", 
    messSet = character(), colname = "Messages",
    qfields = c("app", "uid"), ...)
}
\arguments{
  \item{messages}{ A list into which to add the messages.}
  \item{sender}{ A character value used as the \code{\link{sender}}
    field of the message.}
  \item{dbname}{A character value giving the name of the database in
    which to put the message. See \code{\link[mongolite]{mongo}}. }
  \item{dburi}{A character vector giving the URI for the database.  See
    \code{\link[mongolite]{mongo}}. }
  \item{messSet}{A character vector giving the message values of the messages
    that will be processed.  Messages whose \code{\link{mess}} value are
    not in this list will be ignored by this listener.}
  \item{colname}{The name of the database column into which the messages
    will be sent.  See \code{\link[mongolite]{mongo}}.}
  \item{targetField}{The name of the field that will be modified in the
    database by the \code{\linkS4class{UpdateListener}}. }
  \item{jsonEncoder}{A function that will be used to encode the data
    object as JSON before it is set.  See \code{\linkS4class{UpdateListener}}. }
  \item{qfields}{The fields that will be used as a key when trying to
    find matching messages in the database for the
    \code{\linkS4class{UpsertListener}}. } 
  \item{\dots}{Other arguments passed to the constructor. }
}
\details{

  The functions are as follows:
  \describe{
    \item{\code{CaptureListener}}{Creates an object of class
      \code{\linkS4class{CaptureListener}} which stores the messages in
      a list.}
    \item{\code{InjectionListener}}{Creates an object of class
      \code{\linkS4class{InjectionListener}} which inserts the message into
      the designated database.} 
    \item{\code{UpdateListener}}{Creates an object of class
      \code{\linkS4class{UpdateListener}} which updates the designated
      field.}
    \item{\code{UpsertListener}}{Creates an object of class
      \code{\linkS4class{UpsertListener}} which insert or replaces the
      message in the designated collection.} 
  }
  See the class descriptions for more information.

}
\value{

  An object of the virtual class \code{\link{Listener}}.

}
\references{

  This is an example of the observer design pattern.
  \url{https://en.wikipedia.org/wiki/Observer_pattern}. 

}
\author{Russell Almond}
\seealso{
  \code{\link{Listener}}, \code{\linkS4class{P4Message}},
  \code{\linkS4class{UpsertListener}},
  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{CaptureListener}},
  \code{\linkS4class{InjectionListener}},
  \code{\linkS4class{ListenerSet}},
  \code{\link[mongolite]{mongo}}
}
\examples{

cl <- CaptureListener()

il <- InjectionListener(sender="EI_app",
            dbname="EARecords",dburi="mongodb://localhost",
            colname="EvidenceSets",messSet="New Observables")

upsl <- UpsertListener(sender="EI_app",
            dbname="EARecords",dburi="mongodb://localhost",
            colname="LatestEvidence",messSet="New Observables",
            qfields=c("app","uid"))

trophy2json <- function(dat) {
  paste('{', '"trophyHall"', ':','[',
        paste(
            paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
                  sep=""), collapse=", "), '],',
        '"bankBalance"', ':', dat$bankBalance, '}')
}
ul <- UpdateListener(dbname="Proc4",dburi="mongodb://localhost",
            colname="Players",targetField="data",
            messSet=c("Money Earned","Money Spent"),
            jsonEncoder="trophy2json")


}
\keyword{ interface }
\keyword{ database }
