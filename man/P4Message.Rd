\name{P4Message}
\alias{P4Message}
\alias{m_id}
\alias{app}
\alias{uid}
\alias{mess}
\alias{context}
\alias{sender}
\alias{timestamp}
\alias{details}
\alias{toString,P4Message-method}
\alias{show,P4Message-method}
\alias{all.equal.P4Message}
\title{Constructor and accessors for P4 Messages}
\description{

  The function \code{P4Message()} creates an object of class
  \code{"\linkS4class{P4Message}"}.  The other functions access fields
  of the messages.

}
\usage{
P4Message(uid, context, sender, mess, timestamp = Sys.time(), details = list(), app = "default", processed=FALSE)
m_id(x)
app(x)
uid(x)
mess(x)
context(x)
sender(x)
timestamp(x)
details(x)
\S4method{toString}{P4Message}(x,...)
\S4method{show}{P4Message}(object)
\S3method{all.equal}{P4Message}(target, current, ..., checkTimestamp = FALSE,
                                check_ids = TRUE)
}
\arguments{
  \item{uid}{A character object giving an identifier for the user or student.}
  \item{context}{A character object giving an identifier for the
    context, task, or item.}
  \item{sender}{A character object giving an identifier for the sender.
    In the four-process architecture, this should be one of
    \dQuote{Activity Selection Process}, \dQuote{Presentation Process},
    \dQuote{Evidnece Identification Process}, or \dQuote{Evidence
      Accumulation Process}.}
  \item{mess}{A character object giving a message to be sent.}
  \item{timestamp}{The time the message was sent.}
  \item{details}{A list giving the data to be sent with the message.}
  \item{app}{An identifier for the application using the message.}
  \item{processed}{A logical flag:  true if the message has been
    processed and false otherwise.}
  \item{x}{A message object to be queried, or converted to a string.}
  \item{...}{Addtional arguments for \code{\link[methods]{show}} or
    \code{\link[base]{all.equal}}.}
  \item{object}{A message object to be converted to a string.}
  \item{target}{A P4Message to compare.}
  \item{current}{A P4Message to compare.}
  \item{checkTimestamp}{Logical flag.  If true, the timestamps are
    compared as part of the equality test.}
  \item{check_ids}{Logical flag.  If true, the database ids are
    compared as part of the equality test.}
  
  
}
\details{

  This class represents a semi-structured data object with certain
  header fields which can be indexed plus the free-form \code{details()}
  field which contains the body of the message.  It can be serielized in
  JSON format (using \code{\link{as.json}}
  in the Mongo database (using the
  \code{\link[mongolite]{mongo}lite} package). 

  Using the public methods, the fields can be read but not set.  The
  generic functions are exported so that other object can extend the
  \code{P4Message} class.  The \code{m_id} function accesses the mongo
  ID of the object (the \code{_id} field).

  The function \code{all.equal.P4Message} checks two messages for
  identical contents.  The flags \code{checkTimestamp} and
  \code{check_ids} can be used to suppress the checking of those
  fields.  If timestamps are checked, they must be within .1 seconds to
  be considered equal.

}
\value{
  An object of class \code{\linkS4class{P4Message}}.

  The \code{app()}, \code{uid()}, \code{context()}, \code{sender()}, and
  \code{mess()} functions all return a character scalar.  The
  \code{timestamp()}, function returns an object of type \code{POSIXt}
  and the \code{details()} function returns a list.

  The function \code{all.equal.P4Message} returns either ‘TRUE’  or a
  vector of mode "character" describing the differences between
  \code{target} and \code{current}.

}
\references{

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 


}
\author{Russell G. Almond}
\seealso{
  \code{\linkS4class{P4Message}} --- class
  \code{\link{parseMessage}}, \code{\link{saveRec}},
  \code{\link{getOneRec}}

}
\examples{

mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:15:25 EST"),
         list(correct=TRUE,selection="D"))
stopifnot(
  app(mess1) == "default",
  uid(mess1) == "Fred",
  context(mess1) == "Task 1",
  sender(mess1) == "Evidence ID",
  mess(mess1) == "Scored Response",
  timestamp(mess1) == as.POSIXct("2018-11-04 21:15:25 EST"),
  details(mess1)$correct==TRUE,
  details(mess1)$selection=="D"
)

mess2 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
         as.POSIXct("2018-11-04 21:15:25 EST"),
         list(correct=FALSE,selection="E"))
all.equal(mess1,mess2)
stopifnot(!isTRUE(all.equal(mess1,mess2)))

}

\keyword{ classes }

