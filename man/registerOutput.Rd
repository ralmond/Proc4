\name{registerOutput}
\alias{registerOutput}
\alias{registerOutput,ListenerSet-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Registers a file used for output information from an engine.}
\description{

  Many scoring engines provide output data files, or log file.  This
  function registers the output files in a database collection, so that
  other functions can find them.

}
\usage{
registerOutput(registrar, name, filename, app, process, type = "data", doc = "")
}
\arguments{
  \item{registrar}{The \code{\linkS4class{ListenerSet}} responsible for
    registering the the file.}
  \item{name}{A character scalar identifying the data file.}
  \item{filename}{A character scalar giving the path to the file.}
  \item{app}{A character scalar identifying the application}
  \item{process}{A character scalar identifying the name of the process
    (engine) generating the data).}
  \item{type}{A character scalar identify the data type.  Currently
    supported values are \dQuote{data} for data files in csv format, and
    \dQuote{log} for log files.}
  \item{doc}{An object of type character describing the file.}
}
\details{

  The file \code{system.file("dongle/Status.php",package="Proc4")}
  provides a web interface listing the output files.  It generates this
  by looking at the \dQuote{OutputFile} collection in the \dQuote{Proc4}
  database.  It then builds links to the files, so they can be
  downloaded.

  The \code{registerOutput} method is used to add, or update the date on
  files in the collection.

}
\value{
  Mostly used for is side-effects.  Returns information about the
  success of the database operation.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{ListenerSet}}, \code{\link{listenerDataTable}}
}
\examples{

\dontrun{
jspecs <- '{
     "listeners":[
	{
	    "name":"ToAS",
	    "type":"InjectionListener",
	    "dbname":"ASRecords",
	    "colname":"Statistics",
	    "messages":["Statistics"]
	}

    ]}'

speclist <- jsonlite::fromJSON(jspecs,FALSE)
appid <- "ecd://pluto.coe.fsu.edu/P4Test"
outdir <- "/usr/local/share/Proc4/data"

lset <- buildListenerSet("TestEngine",speclist$listeners,
                         appid=appid,
                         lscol="Messages",dbname="test",
                         dburi="", sslops=mongolite::ssl_options(),
                         registrycol="OutputFiles",
                         registrydbname="Proc4")


## After engine running.

sl <- lset$listeners[["ToAS"]]
sdat <- listenerDataTable(sl,NULL,appid)
registerOutput(ls,"PP Statistics",
               file.path(outdir,"PPstats.csv"),
               appid,"EA")

}
}
\keyword{ interface }
\keyword{ database }
\concept{ listener }

