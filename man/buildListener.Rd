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
buildListener(specs, app, dburi)
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
    \item{dburi}{If this field is non-null, then the \dQuote{dburi}
      field of the resulting listener will be set to \code{dburi}.
      Typically, this field should have the value "<dburi>" if present.}
    \item{dbname}{The name of the database in which the messages will be
      recorded.}
    \item{dbname}{The name of the database collection in which the
      messages will be recorded.}
    \item{messages}{A character vector giving the names of the messages
      the listener will pay attention to.  Note that this maps to the
      field \dQuote{messSet} in the listener object.}
    \item{targetField}{Used in the \code{\linkS4class{UpdateListner}}
      and \code{\linkS4class{UpsertListner}} to indicate the field to be
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
	    "sender":"EI_<app>",
	    "dburi":"<dburi>",
	    "dbname":"EARecords",
	    "colname":"EvidenceSets",
	    "messages":["New Observables"]
	},
	{
	    "name":"PPPersistantData",
	    "type":"UpdateListener",
	    "dburi":"<dburi>",
	    "dbname":"Proc4",
	    "colname":"Players",
	    "targetField":"data",
	    "jsonEncoder":"trophy2json",
	    "messages":["Money Earned", "Money Spent"]
	}
    ]'

speclist <- fromJSON(jspecs,FALSE)

l1 <- buildListener(speclist[[1]],"test","mongodb://localhost")
stopifnot (isListener(l1),name(l1)=="ppLStest",
           is(l1,"TableListener"),
           match("Coins Spent",l1$messSet,nomatch=0)>0)

l2 <- buildListener(speclist[[2]],"test","mongodb://localhost")
stopifnot (isListener(l2),name(l2)=="ToEA",
           is(l2,"InjectionListener"),
           l2$sender=="EI_test",
           l2$dburi=="mongodb://localhost",
           match("New Observables",l2$messSet,nomatch=0)>0)


l3 <- buildListener(speclist[[3]],"test","mongodb://localhost")
stopifnot (isListener(l3),name(l3)=="PPPersistantData",
           is(l3,"UpdateListener"),
           l3$dburi=="mongodb://localhost",
           match("Money Earned",l2$messSet,nomatch=0)>0)


}
\keyword{ interface }

