\name{resetListeners}
\alias{resetListeners}
\alias{resetListeners,ListenerSet-method}
\title{Clears messages caches associated with listeners}
\description{

  Listeners often cache the messages in some way.  This causes the
  message cache to be cleared, and operation which is often useful
  before a rerun.  The \code{which} argument is used to control which
  listeners should have their cache cleared.
  
}
\usage{
resetListeners(x, which, app)
\S4method{resetListeners}{ListenerSet}(x, which, app)
}
\arguments{
  \item{x}{A \code{\linkS4class{ListenerSet}} object containing the
    listeners to be reset.}
  \item{which}{A character vector containing the names of the listeners
    to reset.  The special keyword \dQuote{ALL} means all listeners will
    be reset.  The special keyword \dQuote{Self} means that the cache
    associated with the listener set will be reset.}
  \item{app}{A global applicaiton identifier. The reset operation should
    only be applied to messages from  this application.}
}
\details{

  Each \code{\link{Listener}} object (including the listener set)
  has a \code{$reset()} method which empties the cache of messages.  This
  method calls the \code{$reset()} method for each of the listeners
  named in \code{which}.  The special keyword \dQuote{ALL} is used to
  reset all listeners and the special keyword \dQuote{Self} is used to
  refer to the \code{\link{ListenerSet}} object itself (which may have a
  database colleciton).

}
\value{
  The \code{\link{ListenerSet}} object is returned.
}
\author{Russell Almond}
\seealso{

  \code{\link{ListenerSet}}, \code{\link{Listener}}
}
\examples{

\dontrun{## Requires Mongo database set up.

data2json <- function(dat) {
  toJSON(sapply(dat,unboxer))
}

listeners <- list(
  cl = CaptureListener(name="cl"),
  upd = UpdateListener(name="upd",messSet="New Observables",
                         dburi="mongodb://localhost",dbname="test",
                         targetField="data",jsonEncode="data2json",
                         colname="Updated"),
  ups = UpsertListener(name="ups",sender="EIEvent",messSet="New Observables",
                         dburi="mongodb://localhost",dbname="test",
                         colname="Upserted", qfields=c("app","uid")),
  il = InjectionListener(name="il",sender="EIEvent",messSet="New Observables",
                         dburi="mongodb://localhost",dbname="test",
                         colname="Injected"),
  tl = TableListener(name="tl",
                   messSet="New Observables",
                   fieldlist=c(uid="character", context="character",
                               timestamp="character",
                               solvedtime="numeric",
                               trophy="ordered(none,silver,gold)"))
  )
  
lset <- ListenerSet$new(sender="Other",dburi="mongodb://localhost",
                colname="messages",dbname="test",listeners=listeners)

mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EIEvent",mess="New Observables",
                   details=list(trophy="gold",solvedtime=10))

resetListeners(lset,"ALL","default")
receiveMessage(lset,mess1)
## Check recieved messages.
stopifnot(lset$messdb()$count(buildJQuery(app="default"))==1L,
          length(listeners$cl$messages)==1L,
          listeners$upd$messdb()$count(buildJQuery(app="default"))==1L,
          listeners$ups$messdb()$count(buildJQuery(app="default"))==1L,
          listeners$il$messdb()$count(buildJQuery(app="default"))==1L,
          nrow(listeners$tl$returnDF())==1L)

resetListeners(lset,c("Self","cl","il","tl"),"default")
stopifnot(lset$messdb()$count(buildJQuery(app="default"))==0L,
          length(listeners$cl$messages)==0L,
          listeners$upd$messdb()$count(buildJQuery(app="default"))==1L,
          listeners$ups$messdb()$count(buildJQuery(app="default"))==1L,
          listeners$il$messdb()$count(buildJQuery(app="default"))==0L,
          nrow(listeners$tl$returnDF())==0L)

resetListeners(lset,"ALL","default")
stopifnot(lset$messdb()$count(buildJQuery(app="default"))==0L,
          length(listeners$cl$messages)==0L,
          listeners$upd$messdb()$count(buildJQuery(app="default"))==0L,
          listeners$ups$messdb()$count(buildJQuery(app="default"))==0L,
          listeners$il$messdb()$count(buildJQuery(app="default"))==0L,
          nrow(listeners$tl$returnDF())==0L)


}
}
\keyword{ interface}
\keyword{ database }
