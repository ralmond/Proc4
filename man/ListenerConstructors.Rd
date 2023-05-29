\name{ListenerConstructors}
\alias{CaptureListener}
\alias{InjectionListener}
\alias{UpdateListener}
\alias{UpsertListener}
\alias{TableListener}
\title{Constructors for Listener Classes}
\description{

  These functions create objects of class
  \code{\linkS4class{CaptureListener}},  \code{\linkS4class{UpdateListener}},
  \code{\linkS4class{UpsertListener}},
  \code{\linkS4class{InjectionListener}}, and
  \code{\linkS4class{TableListener}}.


}
\usage{
CaptureListener(name="Capture",messages = list(),
                messSet=character(), ...)
InjectionListener(name="Injection", db = mongo::MongoDB(noMongo=TRUE),
    messSet = character(), ...)
UpdateListener(name="Update",db = mongo::MongoDB(noMongo=TRUE),
               targetField = "data", qfields = c("app", "uid"),
               jsonEncoder = "unparseData", messSet=character(), ...)
UpsertListener(name="Upsert", messSet = character(),
               db = mongo::MongoDB(noMongo=TRUE),
               qfields = c("app", "uid"), ...)
TableListener(name = "ppData",
    fieldlist = c(uid = "character", context = "character"),
    messSet = character(), ...)
}
\arguments{
  \item{messages}{ A list into which to add the messages.}
  \item{messSet}{A character vector giving the message values of the messages
    that will be processed.  Messages whose \code{\link{mess}} value are
    not in this list will be ignored by this listener.}
  \item{db}{A \code{\link[mongo]{MongoDB}} object which provides reference to a database collection.}
  \item{targetField}{The name of the field that will be modified in the
    database by the \code{\linkS4class{UpdateListener}}. }
  \item{jsonEncoder}{A function that will be used to encode the data
    object as JSON before it is set.  See \code{\linkS4class{UpdateListener}}. }
  \item{qfields}{The fields that will be used as a key when trying to
    find matching messages in the database for the
    \code{\linkS4class{UpsertListener}}. }
  \item{name}{An object of class \code{character} naming the listener. }
  \item{fieldlist}{A named \code{character} vector giving the
      names and types of the columns of the output matrix.  See
      \code{\linkS4class{TableListener}}. }

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
    \item{\code{TableListener}}{Creates an object of class
      \code{\linkS4class{TableListener}} which adds details from message
      to rows of a data frame.}
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
  \code{\linkS4class{TableListener}},
  \code{\linkS4class{ListenerSet}},
  \code{\link[mongolite]{mongo}}
}
\examples{

cl <- CaptureListener()

il <- InjectionListener("Evidence Collector",
                        db=mongo::MongoDB(collection="EvidenceSets",
                                          db="EARecords",
                                          url = "mongodb://localhost",
                                          noMongo=TRUE),
                        messSet="New Observables")

upsl <- UpsertListener("Save Observables",
                        db=mongo::MongoDB(collection="LatestEvidence",
                                          db="EARecords",
                                          url = "mongodb://localhost",
                                          noMongo=TRUE),
                       messSet="New Observables", qfields=c("app","uid"))

trophy2json <- function(dat) {
  paste('{', '"trophyHall"', ':','[',
        paste(
            paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
                  sep=""), collapse=", "), '],',
        '"bankBalance"', ':', dat$bankBalance, '}')
}
ul <- UpdateListener("Player Data",
                     db=mongo::MongoDB(collection="Players",
                                       db="Proc4",
                                       url = "mongodb://localhost",
                                       noMongo=TRUE),
                     targetField="data",
                     messSet=c("Money Earned","Money Spent"),
                     jsonEncoder="trophy2json")

tabMaker <- TableListener(name="Trophy Table",
                          messSet="New Observables",
                          fieldlist=c(uid="character", context="character",
                                      timestamp="character",
                                      solvedtime="numeric",
                                      trophy="ordered(none,silver,gold)"))

}
\keyword{ interface }
\keyword{ database }
