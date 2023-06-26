\name{listenerDataTable}
\alias{listenerDataTable}
\alias{listenerDataTable,InjectionListener-method}
\title{Fetches a data frame containing information captured by listener}
\description{

  A number of listerner capture information.  This method extracts the
  data as a data frame for further processing.

}
\usage{
listenerDataTable(listener, fields = NULL, appid = character())
\S4method{listenerDataTable}{InjectionListener}(listener, fields = NULL, appid = character())
}
\arguments{
  \item{listener}{A \code{\linkS4class{Listener}} subclass where the
    information was stored.}
  \item{fields}{A list of fields to extract.  If this is \code{NULL}
    (default), then all available feilds are extracted.}
  \item{appid}{The name of the application whose data is to be
    extracted.  (In case data from more than one application is stored
    in the same collection.)}
}
\value{
  A \code{data.frame} giving the requested data.
}
\author{Russell Almond}
\note{

  Currently only implemented for the \code{\link{InjectionListener}},
  which is saving raw \code{\linkS4class{P4Message}}s.  This method
  links the selected data fields with header fields and builds a table.

}
\seealso{
  \code{\linkS4class{InjectionListener}}, \code{\link{registerOutput}}
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
\keyword{ database }
\concept{ listener }

