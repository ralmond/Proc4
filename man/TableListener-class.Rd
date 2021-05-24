\name{TableListener-class}
\Rdversion{1.1}
\docType{class}
\alias{TableListener-class}
\alias{isListener,TableListener-method}
\alias{receiveMessage,TableListener-method}
\alias{listenerName,TableListener-method}

\title{Class \code{"TableListener"}}
\description{

  A listener that captures data from a \code{\linkS4class{P4Message}}
  and puts it into a dataframe.

}
\section{Extends}{

    This class implements the \code{\link{Listener}} interface.

    All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{isListener}{\code{signature(x = "TableListener")}: TRUE }
    \item{receiveMessage}{\code{signature(x = "TableListener")}: If
      the message is in the \code{messSet}, it adds a row to its
      internal table using the fields specified in \code{fieldlist}. 
      (See details.) }
    \item{listenerName}{\code{signature(x = "TableListener")}: returns the name
      of the table.  This is usually also the filename where the table
      will be stored. }
  }
}
\references{
  This is an example of the observer design pattern.
  \url{https://en.wikipedia.org/wiki/Observer_pattern}. 

}
\author{Russell Almond, Lukas Liu, Nan Wang}
\details{

  This listener builds up a data frame with selected data from the
  messages.  What data is captured is controlled by the \code{fieldlist}
  object.  This is a named character vector whose names correspond to
  field names and whose values correspond to type names (see
  \code{\link[base]{typeof}}.  The type can also be one of the two
  special types, \code{ordered} or \code{factor}.  The following is a
  summary of the most common types:
  \describe{
    \item{\code{"numeric"}, \code{"logical"}, \code{"integer"},
      \code{"double"}:}{These are numeric values.}
    \item{\code{"character"}:}{These are character values.  They are not
      converted to factors (see factor types below).}
    \item{\code{"list"},\code{"raw"}, other values returned by
      \code{\link[base]{typeof}}:}{These are usuable, but should be used
      with caution because the output data frame may not be easy to
      export to other program.}
    \item{\code{"ordered(...)"}, \code{"factor(...)":}}{These produce
      objects of type \code{\link[base]{ordered}} and
      \code{\link[base]{factor}} with the comma separated values
      between the parenthesis passed as the \code{levels} argument.
      For example, \code{"ordered(Low,Medium,High)"} will produces
      an ordered factor with three levels.  (Note that levels should
      be in increasing order for ordered factors, but this doesn't
      matter for unordered factors.)}
  }

  For most fields, the field name is matched to the corresponding
  element of the \code{\link{details}} of the messages.  The exceptions
  are the names \code{\link{app}}, \code{\link{context}},
  \code{\link{uid}}, \code{\link{mess}}, \code{\link{sender}},
  \code{\link{timestamp}}, which return the value of the corresponding
  header fields of the message.  Note that 
    

}
\seealso{

  \code{\link{Listener}}, \code{\linkS4class{P4Message}},
  \code{\linkS4class{UpdateListener}}, 
  \code{\linkS4class{InjectionListener}}, 
  \code{\linkS4class{CaptureListener}},
  \code{\linkS4class{UpsertListener}},
  \code{\link{TableListener}},

}
\examples{


mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EIEvent",mess="New Observables",
                   details=list(trophy="gold",solvedtime=10))
tabMaker <- TableListener(name="Trophy Table",
                   messSet="New Observables",
                   fieldlist=c(uid="character", context="character",
                               timestamp="character",
                               solvedtime="numeric",
                               trophy="ordered(none,silver,gold)"))

receiveMessage(tabMaker,mess1)
tabMaker$returnDF()

}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{name}:}{Object of class \code{character} naming the listener. }
    \item{\code{fieldlist}:}{A named \code{character} vector giving the
      names and types of the columns of the output matrix.  See details. }
    \item{\code{df}:}{Object of class \code{data.frame} this is the
      output data frame.  Note that the first line is blank line.  Use
      the function \code{$returnDF()} to get the valid rows. }
    \item{\code{messSet}:}{A vector of class \code{character} giving the
      name of messages which are sent to the database.  Only messages
      for which \code{mess(mess)} is an element of \code{messSet} will
      be added to the table..}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{receiveMessage(mess)}:}{ Processes the message argument. }
    \item{\code{initDF()}:}{ An internal function that sets up the first
      row of the table as a blank line of the proper types.  Called by
      \code{receiveMessage()}.}
    \item{\code{initialize(name, fieldlist, messSet, ...)}:}{
      Initializes the fields. }
    \item{\code{reset(app)}:}{ Calls \code{initDF()} to reset the table.}
    \item{\code{returnDF()}:}{ Returns the part of the \code{df} which
      has data (e.g., omits first line which is used to set the types.)}
  }
}
