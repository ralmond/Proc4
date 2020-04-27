\name{P4Message-class}
\Rdversion{1.1}
\docType{class}
\alias{P4Message-class}
\alias{m_id,ANY-method}
\alias{m_id<-,ANY-method}
\alias{app,P4Message-method}
\alias{as.jlist,P4Message,list-method}
\alias{as.json,P4Message-method}
\alias{context,P4Message-method}
\alias{context<-,P4Message-method}
\alias{details,P4Message-method}
\alias{mess,P4Message-method}
\alias{sender,P4Message-method}
\alias{timestamp,P4Message-method}
\alias{timestamp<-,P4Message-method}
\alias{uid,P4Message-method}
\alias{processed,P4Message-method}
\alias{processingError,P4Message-method}

\title{Class \code{"P4Message"}}
\description{

  This is a message which is sent from one process to another in the
  four process architecture.  There are certain header fields with are
  used to route the message and the details field which is an arbitrary
  list of data which will can be used by the receiver.

  This class represents a semi-structured data object with certain
  header fields which can be indexed plus the free-form \code{details()}
  field which contains the body of the message.  It can be serielized in
  JSON format (using \code{\link{as.json}}) or saved
  in the Mongo database (using the \code{\link[mongolite]{mongo}lite}
  package).  

}
\section{Objects from the Class}{

Objects can be created by calls to the \code{\link{P4Message}()}
function. 

}
\section{Message Queues}{

  Because all messages have a processed flag and a timestamp, a message
  collection becomes a queue.  Simply search for the message with the
  earliest timestamp with \code{\link{processed}(mess)==FALSE} and
  excute that.  Then sets processed equal to true using
  \code{\link{markAsProcessed}}.  

  If an error occurs during processing, the error can be associated with
  the message by setting the pError field using
  \code{\link{markAsError}}.  
  
}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Used for internal database ID.}
    \item{\code{app}:}{Object of class \code{"character"} which
      specifies the application in which the messages exit. }
    \item{\code{uid}:}{Object of class \code{"character"} which
      identifies the user (student). }
    \item{\code{context}:}{Object of class \code{"character"} which
      identifies the context, task, or item. }
    \item{\code{sender}:}{Object of class \code{"character"} which
      identifies the sender.  This is usually one of
      "Presentation Process", "Evidence Identification Process",
      "Evidence Accumulation Process", or "Activity Selection Process".}
    \item{\code{mess}:}{Object of class \code{"character"} a general
      title for the message context.}
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} which gives
      the time at which the message was generated.}
    \item{\code{data}:}{Object of class \code{"list"} which contains the
      data to be transmitted with the message.}
    \item{\code{processed}:}{A logical value:  true if the message has
      been processed, and false if the message is still in queue to be
      processed.  This field is set with \code{\link{markAsProcessed}}.}
    \item{\code{pError}:}{If a error occurs while processing this event,
      information about the error can be stored here, either as an R
      object, or as an R object of class error (or any class).  This
      field is accessed with \code{\link{processingError}} and set with
      \code{\link{markAsError}}. }
  }
}
\section{Methods}{
  \describe{
    \item{m_id}{\code{signature(x = "ANY")}: returns the \code{_id}
      field, the database ID. }
    \item{app}{\code{signature(x = "P4Message")}: returns the app field. }
    \item{as.jlist}{\code{signature(obj = "P4Message", ml = "list")}:
      coerces the object into a list to be processed by
      \code{\link[jsonlite]{toJSON}}. }
    \item{as.json}{\code{signature(x = "P4Message")}: Coerces the
      message into a JSON string. }
    \item{context}{\code{signature(x = "P4Message")}: returns the
      context field. }
    \item{details}{\code{signature(x = "P4Message")}: returns the data
      associated with the message as a list. }
    \item{mess}{\code{signature(x = "P4Message")}: returns the message field. }
    \item{sender}{\code{signature(x = "P4Message")}: returns the sender
      field.}
    \item{timestamp}{\code{signature(x = "P4Message")}: returns the timestamp. }
    \item{uid}{\code{signature(x = "P4Message")}: returns the user ID. }
    \item{processing}{\code{signature(x = "P4Message")}: returns a
      logical value indicated whether or not the message has been marked
      as processed.}
    \item{processingError}{\code{signature(x = "P4Message")}: if an
      error occurred while processing this message, returns a value
      describing the error.  Otherwise, returns NULL.}
  }
}
\references{

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 
}
\author{ Russell G. Almond}

\seealso{
  \code{\link{P4Message}()} --- constructor
  \code{\link{parseMessage}}, \code{\link{saveRec}},
  \code{\link{getOneRec}}
}
\examples{
showClass("P4Message")
}
\keyword{classes}
