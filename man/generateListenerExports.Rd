\name{generateListenerExports}
\alias{generateListenerExports}
\alias{updateTable}
\title{Build tables from messages saved by the listner}
\description{

  The function \code{updateTable} extracts a data table from the
  listener named by \code{which}, saves it into the named file.  It then
  registers the generated file using \code{\link{registerOutput}}.

  The function \code{generateListenerExports} calls the
  \code{updateTable} for each element in the export list, which should
  be a list of arguments to \code{updateTable}.
}
\usage{
generateListenerExports(ls, exportlist, appid, outdir, process = ls$sender)
updateTable(ls, which, type, appid, outdir, fname = "<app>_<name>.csv", 
            process = ls$sender, flattener = jsonlite::flatten, doc="",
            name=which) 
}
\arguments{

  \item{ls}{The \code{\linkS4class{ListenerSet}} which contains both the
    listener and the registry.}
  \item{exportlist}{A list of lists of arguments to \code{updateTable}.}
  \item{appid}{A character scalar giving the name of the application.
    This should be the long name (e.g.,
    \dQuote{ecd://org/unit/assessment} not the short name
    (\dQuote{assessment}). }
  \item{outdir}{The path to the directory where the output should be stored.}
  \item{process}{A character scalare giving the name of the generating
    process.  Passed to \code{\link{registerOutput}}.}
  \item{which}{An itentifier for which listener will generate the table,
    in other words, the name of one of the listeners.}
  \item{type}{A character string identifying the type of the output.
    Passed to \code{\link{registerOutput}}.}
  \item{fname}{A character vector giving a pattern for a file name.  The
    string \dQuote{<app>} is substituted for \code{basename(app)}, the
    string \dQuote{<name>} is substituted for \code{name}.}
  \item{flattener}{A function or string naming a function which is used
    to flatten nested data.  See details.}
  \item{name}{Used to label the table in the registrity.}
  \item{doc}{A doc string added to the registrty.}
}
\details{

  The \code{updateTable} function calls the
  \code{\link{listenerDataTable}} on the listener
  \code{ls$listners[[which]]}. As the \code{\link{details}} fields of
  the messages, could be nested, it might need to be flattened so that
  it can be exported as a CSV file, so the \code{flattener} function is
  called.  Then the resulting data table is written out to
  \code{outdir/fname}.

  The \code{generateListenerExports} is fed a list of arguments for
  \code{updateTable}.  The idea is that this information can be included
  in the \code{config.json} file.  Each element should be a list with
  the following components:
  \describe{
    \item{which}{Required, the name of the listener.}
    \item{type}{Optional, the type of the output (for the registry);
      defaults to \dQuote{data}.}
    \item{name}{Optional, the name of table in the registry.  Defaults
      to \code{which}.}
    \item{fname}{Optional, the file name.  This is actually a pattern,
      and \dQuote{<app>} is replaced with \code{basename(appid)} and
      \dQuote{<name>} is replaced with \code{name}.  Default is
      \dQuote{<app>_<name>.csv}.}
    \item{flattener}{Optional, The name of the flatterner function.
      Defaults to \code{\link[jsonlite]{flatten}}.}
    \item{doc}{Optional, a character string describing the table in the
      registry.} 
  }
  Note that the \code{appid}, \code{outdir} and \code{process} fields
  are taken from the call to \code{generateListenerExports}.

}
\section{Flattening Complex Data}{

  The data stored in the messages can in fact be nested deeply.  So the
  raw dataframe returned by \code{\link{listenerDataTable}} could have
  columns that are themselves data frames.  The function
  \code{jsonlite::\link[jsonlite]{flatten}} function unrolls these
  columns into individual components.

  Another frequenly used function is
  \code{Peanut::\link[Peanut]{flattenStats}}.
  In particular, the \code{\link[Peanut]{PnodeMargin}} statistic returns
  a labeled vector as output.  This function splits it into columns with
  headers \emph{name}.\emph{state}.  Note that to call a function from
  another package, that package must be named, so a call to
  \code{\link[base]{require}} is in order.

}
\value{

  These functions are mainly used for their side effects.  The
  \code{updateTable} function returns the generated table invisibly, or
  \code{NULL} if \code{\link{listenerDataTable}} returns \code{NULL}.
  The \code{generateListenerExports} returns the last exported table.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{ListenerSet}}, \code{\link{Listener}}

  \code{\link{listenerDataTable}}, \code{\link{registerOutput}}

  \code{\link[jsonlite]{flatten}}, \code{\link[Peanut]{flattenStats}}

}
\examples{

\dontrun{

  config.json <-
    '"listeners":[
	{"name":"ToAS",
	 "type":"InjectionListener",
	 "dbname":"ASRecords",
	 "colname":"Statistics",
	 "jsonEncoder":"unparseData",
	 "jsonDecoder":"parseData",
	 "messages":["Statistics"]
	},
	{"name":"PPStats",
	 "type":"UpdateListener",
	 "targetField":"data",
	 "jsonEncoder":"stats2json",
	 "colname":"Statistics",
	 "messages":["Statistics"]
	}
	    
    ],
    "listenerExports":[
	{"which":"PPStats",
	 "type": "data",
	 "fname":"stats-<app>.csv",
	 "flattener":"flattenStats",
	 "doc": "Reporting statistics"
        },
	{"which":"ToAS",
	 "type": "hist",
	 "fname":"hist-<app>.csv",
	 "flattener":"flattenStats",
	 "doc": "History of history variables."
        }
    ]'
  config <- jsonlite::fromJSON(config.json,FALSE)
  appid <- "ecd://example.edu/testgroup/test"
  outdir <- tempdir()
  ls <- buildListenerSet("EA",config$listeners, appid,
                         lscol="Messages",dbname="test",
                         dburi=mongo::makeDBuri(),
                         sslops=mongolite::ssl_options(),
                         registrycol="files",registrydbname="test")
  ## Need to make sure Peanut::flattenStats is recognized
  require(Peanut)

  updateTable(ls,"PPstats","data",appid,outdir)

  generateListenerExports(ls,config$listenerExports,appid,outdir)

}

}
\keyword{ interface }
\concept{ listener }

