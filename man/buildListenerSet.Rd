\name{buildListenerSet}
\alias{buildListenerSet}
\title{Builds Listener Set from a a JSON configuration}
\description{

  This method builds a \code{\linkS4class{ListenerSet}} for an engine.
  In particular, the \code{config} is list which come from reading a
  JSON file (see \code{\link[jsonlite]{fromJSON}}) which contains the rules for
  building the \code{\linkS4class{Listener}}s in the set.
}
\usage{
buildListenerSet(sender, config, appid, lscol, dbname, dburi,
                 sslops, registrycol, registrydbname,
                 mongoverbose = FALSE)

}
\arguments{
  \item{sender}{A character scalar identifying the message sender.}
  \item{config}{A named list providing details of the
  contained listeners.}
  \item{appid}{A character scalar giving the application ID for the
  application being built.}
  \item{lscol}{A character scalar giving the name of the collection used
  for logging messages by the message set.}
  \item{dbname}{A character scalar giving the name of the database for
  the message log, as well as the default database for listeners.}
  \item{dburi}{A character scalar giving the URI of the mongo collection.}
  \item{sslops}{A list giving options for a SSL connection.  See \code{\link[mongolite]{ssl_options}}.}
  \item{registrycol}{A character scalar giving the name of the
  colleciton for registering output.}
  \item{registrydbname}{A character scalar giving the name of the
  database in which the output registriation collection}
  \item{mongoverbose}{A flag for adding debugging information to Mongo
  calls (see \code{\link[mongo:MongoDB-class]{MongoDB}}).}
}
\details{

  This method builds the listener set starting by calling
  \code{\link{buildListener}(config[[i]])} for each element of the
  \code{config} list.  This then becomes the \code{listeners} to the
  \code{\linkS4class{ListenerSet}} constructor.

  Note that the \code{appid}, \code{dburi}, \code{dbnmae} (mapped to
  \code{defaultDB}), and \code{sslops} are passed to
  \code{buildListeners} to use for defaults.

}
\value{
  An object of class \code{\linkS4class{ListenerSet}}.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{ListenerSet}}, \code{\link{buildListener}}
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
	},
	{
	    "name":"PPStats",
	    "type":"UpdateListener",
	    "dbname":"Proc4",
	    "colname":"Statistics",
	    "targetField":"data",
	    "jsonEncoder":"stats2json",
	    "messages":["Statistics"]
	}

    ]}'

speclist <- jsonlite::fromJSON(jspecs,FALSE)

lset <- buildListenerSet("TestEngine",speclist$listeners,
                         "ecd://pluto.coe.fsu.edu/P4Test",
                         lscol="Messages",dbname="test",
                         dburi="", sslops=mongolite::ssl_options(),
                         registrycol="OutputFiles",
                         registrydbname="test")
}
}
\keyword{ interface }
\concept{ listener }
