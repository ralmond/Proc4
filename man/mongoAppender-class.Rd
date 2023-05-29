\name{mongoAppender-class}
\Rdversion{1.1}
\docType{class}
\alias{mongoAppender-class}
\alias{mongoAppender}

\title{Class \code{"mongoAppender"}}
\description{
  This implements the \code{\link[futile.logger:flog.appender]{appender}} protocol logging to a database.
  Note that \code{flog.appender} expects a function as its argument.  The \code{$logger()} method returns
  a function which can be passed to \code{flog.appender}.

}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\author{ Russell Almond}
\seealso{
  \code{\link[futile.logger]{flog.appender}}
  \code{\link[mongo:MongoDB-class]{MongoDB}}
}
\examples{
  col <- mongo::MongoDB("ErrorLog","Admin",noMongo=TRUE)
  logfile <- tempfile("testlog","/tmp",fileext=".log")
  apnd <- mongoAppender(db=col,app="p4test",engine="Tester",tee=logfile)
  futile.logger::flog.appender(apnd$logger(),"TEST")
}
\keyword{classes}
\keyword{databases}
\section{Fields}{
  \describe{
    \item{\code{db}:}{Object of class \code{JSONDB} the refernce to the column where the log will be stored. }
    \item{\code{app}:}{Object of class \code{character} The application identifier for which we are logging errors.
      (See \code{\link{app}()}).}
    \item{\code{engine}:}{Object of class \code{character} giving the name of the processes (in the 4 Process sense)
      that is generating the messages.}
    \item{\code{tee}:}{Object of class \code{character} if this has length greater than zero, it should be a file
      to which the log is also sent.  If it is \code{""}, then the log message is sent to standard output.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{logit(line)}:}{ This does the work of logging a line. }
    \item{\code{logger()}:}{ This returns a function which does the logging. }
  }
}
