
#################################################
## UpsertListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

#' Class \code{"UpsertListener"}
#'
#'
#' This listener takes messages that match its incomming set and inject them
#' into another Mongo database (presumably a queue for another service).  If a
#' matching message exists, it is replaced instead.
#'
#'
#' The database is a \code{\link[mongolite]{mongo}} collection identified by
#' \code{dburi}, \code{dbname} and \code{colname} (collection within the
#' database).  The \code{mess} field of the \code{\link{P4Message}} is checked
#' against the applicable messages in \code{messSet}.  If it is there, then the
#' message is saved in the collection.
#'
#' Before the message is saved, the collection is checked to see if another
#' message exits which matches on the fields listed in \code{qfields}.  If this
#' is true, the message in the database is replaced.  If not, the message is
#' inserted.
#'
#' @name UpsertListener-class
#' @aliases UpsertListener-class isListener,UpsertListener-method
#' receiveMessage,UpsertListener-method listenerName,UpsertListener-method
#' @docType class
#' @section Extends:
#'
#' This class implements the \code{\link{Listener}} interface.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\link{UpsertListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{CaptureListener}},
#' \code{\linkS4class{InjectionListener}}, \code{\linkS4class{TableListener}},
#' \code{\link[mongolite]{mongo}}
#' @references
#'
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#'
#' \dontrun{
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EABN",mess="Statistics",
#'                    details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
#' ul <- UpsertListener(colname="Statistics",qfields=c("app","uid"),
#'          messSet=c("Statistics"))
#' receiveMessage(ul,mess1)
#' }
#'
UpsertListener <-
  setRefClass("UpsertListener",
              fields=c(name="character",
                       sender="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       qfields="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Upsert",
                             sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(name=name,sender=sender,db=NULL,
                                dburi=dburi,dbname=dbname,
                                colname=colname,messSet=messSet,
                                qfields=qfields,
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
                      flog.debug("Updating record for %s: %s",uid(mess),toString(mess))
                      flog.debug(".. from %s",sender)
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mess@sender <- sender
                      mess@"_id" <- NA_character_
                      query <- lapply(qfields,function(f) do.call(f,list(mess)))
                      names(query) <- qfields
                      messdb()$replace(do.call(buildJQuery,query),
                                       as.json(mess,serialize=TRUE),upsert=TRUE)
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

UpsertListener <- function (name="Upsert",
                            sender="sender",
                            dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            qfields=c("app","uid"),...) {
  new("UpsertListener",name=name,sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,messSet=messSet,qfields=qfields,...)
}

setMethod("isListener","UpsertListener",function(x) TRUE)
setMethod("receiveMessage","UpsertListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","UpsertListener",function(x) x$name)


