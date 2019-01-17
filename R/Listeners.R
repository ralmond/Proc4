## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.

setGeneric("receiveMessage",function(x,mess) standardGeneric("receiveMessage"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setMethod("isListener","ANY",function(x) FALSE)


ListenerSet <-
  setRefClass("ListenerSet",
              fields=c(app="character",
                       dburi="character",
                       colname="character",
                       listeners="list",
                       messdb="mongo",
                       debug="logical"),
              methods = list(
                  initialize =
                    function(app="default",
                             dburi="mongo://localhost:271017/EIRecords",
                             listeners=list(),colname="Messages",
                             debug=FALSE,
                             ,...) {
                      messdb <- mongo(colname,dburi)
                      callSuper(app=app,db=db,dburi=dburi,
                                colname=colname,listeners=listeners,
                                debug=debug,...)
                    }
              ))


## Listener/Message Methods
ListenerSet$methods(
                addListener <- function (name,listener) {
                  if (!isListener(listener)) {
                    stop(listener,"is not a listener.")
                  }
                  listners[name] <<- listener
                },
                removeListener <- function (name) {
                  listners[name] <<- NULL
                },
                notifyListeners <- function (mess) {
                  mess <- saveRec(mess,messdb)
                  if (debug)
                    print("Sending Message ",mess)
                  for (name in names(listeners)) {
                    if (debug) print("...to ",name)
                    receiveMessage(listeners[[name]],mess)
                  }
                }
            )
