

#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.
#' Class \code{"InjectionListener"}
#'
#'
#' This listener takes messages that match its incomming set and inject them
#' into another Mongo database (presumably a queue for another service).
#'
#'
#' The database is a \code{\link[mongolite]{mongo}} collection identified by
#' \code{dburi}, \code{dbname} and \code{colname} (collection within the
#' database).  The \code{mess} field of the \code{\link{P4Message}} is checked
#' against the applicable messages in \code{messSet}.  If it is there, then the
#' message is inserted into the collection.
#'
#' @name InjectionListener-class
#' @aliases InjectionListener-class isListener,InjectionListener-method
#' receiveMessage,InjectionListener-method
#' listenerName,InjectionListener-method
#' @docType class
#' @section Extends:
#'
#' This class implements the \code{\link{Listener}} interface.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\link{InjectionListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{UpsertListener}}, \code{\linkS4class{CaptureListener}},
#' \code{\linkS4class{TableListener}}, \code{\link[mongolite]{mongo}}
#' @references
#'
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#'
#' \dontrun{
#'
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EIEvent",mess="New Observables",
#'                    details=list(trophy="gold",solvedtime=10))
#' ilwind <- InjectionListener(sender="EIEvent",messSet="New Observables")
#' receiveMessage(ilwind,mess1)
#'
#' }
#'
InjectionListener <-
  setRefClass("InjectionListener",
              fields=c(name = "character",
                       sender="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Injection",
                             sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             ...) {
                      callSuper(name=name,sender=sender,db=NULL,
                                dburi=dburi,dbname=dbname,
                                colname=colname,messSet=messSet,
                                ...)
                    },
                  messdb = function () {
                    if (is.null(db)) {
                      db <<- mongolite::mongo(colname,dbname,dburi)
                    }
                    db
                  },
                  receiveMessage = function (mess) {
                    if (mess(mess) %in% messSet) {
                      flog.debug("Sending message %s",toString(mess))
                      flog.debug(".. from %s",sender)
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mess@sender <- sender
                      mess@"_id" <- NA_character_
                      messdb()$insert(as.json(mess,serialize=TRUE))
                    } else {
                      flog.debug("%s ignoring message %s",toString(sender),
                                 toString(mess))
                    }
                  },
                  reset = function(app) {
                    if (!is.null(messdb()))
                      messdb()$remove(buildJQuery(app=app))
                  }
              ))

InjectionListener <- function (name="Injection",
                               sender="sender",
                               dbname="test",
                               dburi="mongodb://localhost",
                               messSet=character(),
                               colname="Messages",...) {
  new("InjectionListener",name=name,sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,messSet=messSet,...)
}

setMethod("isListener","InjectionListener",function(x) TRUE)
setMethod("receiveMessage","InjectionListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","InjectionListener",function(x) x$name)

