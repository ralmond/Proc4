\name{UpdateListener-class}
\Rdversion{1.1}
\docType{class}
\alias{UpdateListener-class}
\alias{isListener,UpdateListener-method}
\alias{receiveMessage,UpdateListener-method}
\alias{listenerName,UpdateListener-method}
\alias{listenerDataTable,UpdateListener-method}
\title{Class \code{"UpdateListener"}}
\description{

  This \code{\link{Listener}} updates an existing record (in a Mongo
  collection) for the student
  (\code{uid}), with the contents of the data (details) field of the
  message.

}
\section{Extends}{

  This class implements the \code{\link{Listener}} interface.

 All reference classes extend and inherit methods from
 \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "UpdateListener")}: TRUE }
    \item{receiveMessage}{\code{signature(x = "UpdateListener", message)}: If
      the message is in the \code{messSet}, it updates the record
      corresponding to \code{app(mess)} and \code{uid(mess)} in the
      database with the contents of \code{details(mess)}.  (See details.) }
    \item{listenerName}{\code{signature(x = "UpdateListener")}: Returns
      the name assigned to the listener.}
    \item{listenerDataTable}{\code{signature(listener =
	"UpdateListener", appid )}: Builds a data datable from the messages.}
  }
}
\details{

  The database is a \code{\link[mongolite]{mongo}} collection identified
  by \code{dburi}, \code{dbname} and \code{colname} (collection within
  the database).  The \code{mess} field of the \code{\link{P4Message}}
  is checked against the applicable messages in \code{messSet}.  If it
  is there, then the record in the database corresponding to the
  \code{qfields} (by default \code{app(mess)} and \code{uid(mess)}) is
  updated.  Specifically, the field \code{targetField} is set to
  \code{details(mess)}.  The function \code{jsonEncoder} is called to
  encode the target field as a JSON object for injection into the
  database.

  If \code{targetField=""}, then the behavior is slightly different.  Instead the fields in
  \code{details(mess)} (labeled by their names) are updated.

}
\section{Data Table}{

  When the \code{\link{listenerDataTable}} method is called, the table
  is made by applying \code{\link[mongo]{mdbFind}} to the target column.
  The behavior is different depending on whether or not a
  \code{targetField} is specified.  If there is no target field, then
  all fields of the column are returned.

  If there is a \code{targetField}, then the \code{jsonDecoder} function
  is applied to its value, and it is joined with the \code{app},
  \code{uid}, \code{context}, \code{timestamp} fields from the header to
  make the data table.

}
\references{

  This is an example of the observer design pattern.
  \url{https://en.wikipedia.org/wiki/Observer_pattern}.

}
\author{Russell Almond}
\seealso{
  \code{\link{Listener}}, \code{\linkS4class{P4Message}},
  \code{\link{UpdateListener}},
  \code{\linkS4class{InjectionListener}},
  \code{\linkS4class{CaptureListener}},
  \code{\linkS4class{UpsertListener}},
  \code{\linkS4class{TableListener}},
  \code{\link[mongolite]{mongo}}

  The function \code{\link{unparseData}} is the default encoder.
}
\examples{

## Updating the data field.
fm <- mongo::fake_mongo(count=list(0L,1L))
ul <- UpdateListener("tester", db=fm,messSet=c("Scored Response"))

## New message, insert
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
receiveMessage(ul,mess1)

## Message is update, update.
mess1a <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:40 EST"),
                     list(correct=TRUE,selection="D",key="D"))
receiveMessage(ul,mess1a)

### No target field, details are added to record.

data2json <- function(dat) {
  jsonlite::toJSON(mongo::unboxer(dat))
}

upwind <- UpdateListener(messSet=c("Money Earned","Money Spent"),
                         db=mongo::MongoDB("Players",noMongo=TRUE),
                         targetField="",
                         jsonEncoder="data2json")

mess2 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EIEvent",mess="Money Earned",
                   details=list(trophyHall=list(list("Down Hill"="gold"),
                                                list("Stairs"="silver")),
                                bankBalance=10))

receiveMessage(upwind,mess2)

}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the Mongo database }
    \item{\code{dburi}:}{Object of class \code{character} giving the url
      of the Mongo database.}
    \item{\code{colname}:}{Object of class \code{character} giving the
      column of the Mongo database. }
    \item{\code{messSet}:}{A vector of class \code{character} giving the
      name of messages which are sent to the database.  Only messages
      for which \code{mess(mess)} is an element of \code{messSet} will
      be inserted.}
    \item{\code{db}:}{Object of class \code{MongoDB} giving the
      database.  Use \code{messdb()} to access this field to makes sure
      it has been set up.}
    \item{\code{qfields}:}{Object of class \code{character} giving the
      names of the fields which should be considered a key for the
      messages.}
    \item{\code{targetField}:}{Object of class \code{character} naming
      the field which is to be set. }
    \item{\code{jsonEncoder}:}{A \code{function} or a non-empty
      \code{character} scalar naming a function which will be used to encode
      \code{details(mess)} as a JSON object.  The default is
      \code{\link{unparseData}}.} 
    \item{\code{decoderEncoder}:}{A \code{function} or \code{character}
      scalar naming a function which will be used to decode
      the target field when building a data table.
      The default is \code{\link{parseData}}.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{messdb()}:}{Accessor for the database collection.
      Initializes the connection if it has not been set up.}
    \item{\code{receiveMessage(mess)}:}{ Does the work of updating the
      database.  See Details. }
    \item{\code{reset(app)}:}{ Empties the database collection of
      messages with this app id.}
    \item{\code{initialize(sender, dbname, dburi, colname, messSet,
        ...)}:}{Sets default values for fields.}
  }
}
