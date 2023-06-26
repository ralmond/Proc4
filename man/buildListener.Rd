\name{buildListener}
\alias{buildListener}
\title{Builds a listener from a JSON description.}
\description{

  This is used in configuration, it will build a listener from a JSON
  description of the listener.  The \dQuote{name} and \dQuote{type}
  fields are required.  The other fields should match the arguments for
  the constructor, with the exceptions noted below:

}
\usage{
buildListener(specs, app, dburi, defaultDB="Proc4",
              ssl_options=mongolite::ssl_options(),
              noMongo = !missing(dburi) && length(dburi) > 0L && nchar(dburi) > 0L)
}
\arguments{
  \item{specs}{A named list (from the JSON) containing the instructions
    for building the listener.
}
  \item{app}{A character value that will get substituted for the string
    \dQuote{<app>} in the \dQuote{name} and \dQuote{sender} fields.}
  \item{dburi}{If a database is used for this listener, then this is the
    uri for the connection.  Note that this is specified in the code and
    not in the JSON.}
  \item{defaultDB}{The name of the database with which the Listener will interact,
  only used if no \code{dbname} field in \code{specs}.}
  \item{ssl_options}{Options used for an SSL connection to the database.
    \code{\link[mongolite]{ssl_options}}.}
  \item{noMongo}{A logical value.  If true, then the connection to the
    Mongo database will not be made, and CRUD operations will basically
    become no-ops.}
}
\details{

  The input to this function is a list that comes from JSON (or some
  other input method that returns a named list).  The \code{specs$type}
  field should be the name of a \code{\link{Listener}} class.  This
  means that \code{specs$type} is the name of a constructor function,
  and the rest of the \code{spec} argument are the arguments.

  Currently, the following fields are used.
  \describe{
    \item{name}{The name of the listener, required.  The string
      \dQuote{<app>} is substituted for \code{app}.}
    \item{type}{Required, the name of the constructor for the desired
      class. The function will generate an error if this does not
      correspond to the name of a class.}
    \item{sender}{A string insterted into logged messages.  The string
      \dQuote{<app>} is substituted for \code{app}.}
    \item{dbname}{The name of the database in which the messages will be
      recorded.  If not present, then the \code{defaultdb} will be used.}
    \item{colname}{The name of the database collection in which the
      messages will be recorded.}
    \item{messages}{A character vector giving the names of the messages
      the listener will pay attention to.  Note that this maps to the
      field \dQuote{messSet} in the listener object.}
    \item{targetField}{Used in the \code{\linkS4class{UpdateListener}}
      and \code{\linkS4class{UpsertListener}} to indicate the field to be
      modified.}
    \item{jsonEncoder}{The name of a function used to encode the field
      value to be modified as JSON. See \code{\link[EABN]{stats2json}}.}
    \item{qfields}{A character vector giving the names of the fields
      used as the key for finding the message to replace.  Usually
      should contain \code{c("uid","app")}.}
    \item{fields}{This should be a named character vector (or list)
      whose names indicate the names of the observables/statistics to
      collect, and whose values are the types.  See
      \code{\linkS4class{TableListener}}; this field maps to the
      \dQuote{fieldlist} field of that class.}
  }
  Other fields in \code{specs} are ignored.

}
\value{

  An object of the virtual class \code{\link{Listener}} (i.e., something
  for which \code{\link{isListener}} should return true.

}
\author{Russell Almond}
\note{

  The field name \dQuote{messages} maps to the internal field
  \code{messSet}.  The field name \dQuote{fields} maps to the internal
  field \code{fieldlist}.

}
\seealso{

  \code{\link{Listener}}, \code{\link[jsonlite]{fromJSON}}

}
\examples{

jspecs <- '[
	{
	  "name":"ppLS<app>",
	  "type":"TableListener",
	  "messages":["Coins Earned","Coins Spent", "LS Watched"],
	  "fields":{
		  "uid":"character",
		  "context":"character",
		  "timestamp":"character",
		  "currentMoney":"numeric",
		  "appId":"numeric",
		  "mess":"character",
		  "money":"numeric",
		  "onWhat":"character",
		  "LS_duration":"difftime",
		  "learningSupportType":"character"
	  }
	},
	{
	    "name":"ToEA",
	    "type":"InjectionListener",
	    "dbname":"EARecords",
	    "colname":"EvidenceSets",
	    "messages":["New Observables"]
	},
	{
	    "name":"PPPersistantData",
	    "type":"UpdateListener",
	    "dbname":"Proc4",
	    "colname":"Players",
	    "targetField":"data",
	    "jsonEncoder":"trophy2json",
	    "messages":["Money Earned", "Money Spent"]
	}
    ]'

speclist <- jsonlite::fromJSON(jspecs,FALSE)

l1 <- buildListener(speclist[[1]],"test",mongo::makeDBuri())

l2 <- buildListener(speclist[[2]],"test",mongo::makeDBuri())

l3 <- buildListener(speclist[[3]],"test",mongo::makeDBuri())

}
\keyword{ interface }
\concept{ listener }

