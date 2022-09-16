
#################################################
## UpdateListener

## This listener updates fields in a reference database for a given
## listener.  It can be used, for example, to keep track of trophies
## earned or balance of money/earned or spent in the game.

#' Class \code{"UpdateListener"}
#'
#'
#' This \code{\link{Listener}} updates an existing record (in a Mongo
#' collection) for the student (\code{uid}), with the contents of the data
#' (details) field of the message.
#'
#'
#' The database is a \code{\link[mongolite]{mongo}} collection identified by
#' \code{dburi}, \code{dbname} and \code{colname} (collection within the
#' database).  The \code{mess} field of the \code{\link{P4Message}} is checked
#' against the applicable messages in \code{messSet}.  If it is there, then the
#' record in the database corresponding to the \code{qfields} (by default
#' \code{app(mess)} and \code{uid(mess)}) is updated.  Specifically, the field
#' \code{targetField} is set to \code{details(mess)}.  The function
#' \code{jsonEncoder} is called to encode the target field as a JSON object for
#' injection into the database.
#'
#' @name UpdateListener-class
#' @aliases UpdateListener-class isListener,UpdateListener-method
#' receiveMessage,UpdateListener-method listenerName,UpdateListener-method
#' @docType class
#' @section Extends:
#'
#' This class implements the \code{\link{Listener}} interface.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\link{UpdateListener}}, \code{\linkS4class{InjectionListener}},
#' \code{\linkS4class{CaptureListener}}, \code{\linkS4class{UpsertListener}},
#' \code{\linkS4class{TableListener}}, \code{\link[mongolite]{mongo}}
#'
#' The function \code{\link{unparseData}} is the default encoder.
#' @references
#'
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#'
#' mess2 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EIEvent",mess="Money Earned",
#'                    details=list(trophyHall=list(list("Down Hill"="gold"),
#'                                                 list("Stairs"="silver")),
#'                                 bankBalance=10))
#' data2json <- function(dat) {
#'   jsonlite::toJSON(sapply(dat,unboxer))
#' }
#'
#' upwind <- UpdateListener(messSet=c("Money Earned","Money Spent"),
#'                          targetField="data",colname="Players",
#'                          jsonEncoder="data2json")
#'
#' receiveMessage(upwind,mess2)
#'
#'
UpdateListener <-
  setRefClass("UpdateListener",
              fields=c(name="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       messSet = "character",
                       qfields="character",
                       targetField="character",
                       jsonEncoder="character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Update",dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             targetField="data",
                             jsonEncoder="unparseData",
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(name=name,db=NULL,
                                dburi=dburi,dbname=dbname,
                                colname=colname,messSet=messSet,
                                targetField=targetField,
                                jsonEncoder=jsonEncoder,
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
                      flog.debug("Updating record for %s (%s): %s",uid(mess),
                                 context(mess), toString(mess))
                      if (nchar(targetField) > 0L) {
                        update <- sprintf('{"$set":{"%s":%s, "context":"%s", "timestamp":%s}}',
                                          targetField,
                                          do.call(jsonEncoder,
                                                  list(details(mess))),
                                          context(mess),
                                          toJSON(unboxer(timestamp(mess)),
                                                 POSIXt="mongo"))
                      } else {
                        update <- sprintf('{"$set":%s}',
                                          do.call(jsonEncoder,
                                                  list(details(mess))))
                      }
                      query <- lapply(qfields,function(f) do.call(f,list(mess)))
                      names(query) <- qfields
                      qq <- do.call(buildJQuery,query)
                      if (messdb()$count(qq) == 0L) {
                        ## Initializize by saving message.
                        flog.trace("Record not found, inserting.")
                        mess@"_id" <- NA_character_
                        messdb()$insert(as.json(mess))
                      } else {
                        flog.trace("Record found, updating.")
                      }
                      ## Insert does not format details, correctly.
                      ## Overwrite with update.
                      flog.trace("Update: %s",update)
                      messdb()$update(qq,update)
                    } else {
                      flog.debug("%s ignoring message %s",dbname,toString(mess))
                    }
                  },
                  reset = function(app) {
                    if (!is.null(messdb()))
                      messdb()$remove(buildJQuery(app=app))
                  }

              ))

UpdateListener <- function (name="Update",dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            targetField="data",
                            qfields=c("app","uid"),
                            jsonEncoder="unparseData",
                            ...) {
  new("UpdateListener",name=name,dbname=dbname,dburi=dburi,messSet=messSet,
      colname=colname,targetField=targetField,jsonEncoder=jsonEncoder,
      qfields=qfields,...)
}

setMethod("isListener","UpdateListener",function(x) TRUE)
setMethod("receiveMessage","UpdateListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","UpdateListener",function(x) x$name)

