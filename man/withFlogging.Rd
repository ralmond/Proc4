\name{withFlogging}
\alias{withFlogging}
\title{Invoke expression with errors logged and traced}
\description{

  This is a version of \code{\link[base]{try}} with a couple of
  important differences.  First, error messages are redirected to the
  log, using the \code{\link[futile.logger]{flog.logger}} mechanisms.
  Second, extra context information can be provided to aid with
  debugging.  Third, stack traces are added to the logs to assist with
  later debugging.

}
\usage{
withFlogging(expr, ..., context = deparse(substitute(expr)), loggername = flog.namespace(), tracelevel = c("WARN", "ERROR", "FATAL"))
}
\arguments{
  \item{expr}{The expression which will be exectued.}
  \item{\dots}{Additional context arguments.  Each additional argument
    should have an explicit name.  In the case of an error or
    warning, the additional context details will be added to the log.}
  \item{context}{A string identifying the context in which the error
    occurred.  For example, it can identify the case which is being
    processed.}
  \item{loggername}{This is passed as the \code{name} argument to
    \code{\link[futile.logger]{flog.logger}}.  It defaults to the
    package in which the call to \code{withFlogging} was made.}
  \item{tracelevel}{A character vector giving the levels of conditions
    for which stack traces should be added to the log.  Should be
    strings with values \dQuote{TRACE}, \dQuote{DEBUG}, \dQuote{INFO},
    \dQuote{WARN}, \dQuote{ERROR} or \dQuote{FATAL}.}
}
\details{

  The various processes of the four process assessment design are meant
  to run as servers.  So when errors occur, it is important that they get
  logged with sufficient detail that they can be reproduced, fixed and
  added to the test suite to prevent recurrance.

  First, signals are caught and redirected to the appropriate
  \code{\link[futile.logger]{flog.logger}} handler.  This has several
  important advantages.  First, the output can be directed to various
  files depending on the origin package.  In general, the name of the
  package should be the name of the logger.  So,
  \code{flog.appender(appender.file("/var/log/Proc4/EIEvent_log.json"),
    name="EIEvent")} would log error from the EIEvent package to the named
  file.  Furthermore, \code{flog.layout(layout.json,name="EIEvent")}
  will cause the log to be in JSON format.

  Second, additional context information is printed when an condition is
  signaled.  The \code{context} string is printed along with the error
  or warning message.  This can be used, for example, to provide
  information about the user and task that was being processed when the
  condition was signaled.  In addition, any of the \code{...} arguments
  are printed.  This can be used to print information about the message
  being processed and the initial state of the system, so that the error
  condition can be reproduced.

  Third, if the class of the exception is in the \code{tracelevel} list,
  then a stack trace will be logged along with the error.  This should
  aid debugging.  

  Fourth, in the case of an error or fatal error, an object of class
  \code{try-error} (see \code{\link[base]{try}}).  Among other things,
  this guarentees that \code{withFlogging} will always return control to
  the next statement.

}
\value{

  If \code{expr} executes successfully (with no errors or fatal errors)
  then the value of \code{expr} will be returned.  If an error occurs
  during execution, then an object of class \code{try-error} will be
  returned. 
}
\references{

  The code for executing the stack trace was taken from
  \url{https://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r}
  

}
\author{Russell Almond}
\seealso{

  \code{\link[base]{try}}, \code{\link[futile.logger]{flog.logger}},
  \code{\link[futile.logger]{flog.layout}},
  \code{\link[futile.logger]{flog.appender}}
}
\examples{

\dontrun{
## Setup to log to file in json format.
flog.appender(appender.file("/var/log/Proc4/Proc4_log.json"),
    name="Proc4")
flog.layout(layout.json,name="EIEvent")
}

xy <- withFlogging(stop("shoes untied"),context="walking",foot="left")
stopifnot(is(xy,"try-error"))


xx <- withFlogging(log(-1))
stopifnot(is.nan(xx))

withFlogging(log(-1),tracelevel=c("ERROR","FATAL"))









}
\keyword{ debugging }
\keyword{ error }
