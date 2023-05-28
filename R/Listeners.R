## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.

setGeneric("receiveMessage",function(x,message) standardGeneric("receiveMessage"))
setGeneric("clearMessages",function(x,app) standardGeneric("clearMessages"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setGeneric("resetListeners",function(x,which,app) standardGeneric("resetListeners"))
setMethod("resetListeners","NULL",function(x,which,app) x)
setMethod("isListener","ANY",function(x) FALSE)
setGeneric("notifyListeners",function(sender,message)
  standardGeneric("notifyListeners"))
setGeneric("listenerName", function (x) standardGeneric("listenerName"))
setGeneric("listeningFor", function (x, newSet) standardGeneric("listeningFor"))

#############################################
## Abstract Listener

RefListener <-
  setRefClass("RefListener",
              fields=c(name = "character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="AbstractListener",
                             db=mongo::MongoDB(noMongo=TRUE),
                             messSet=character(),
                             ...) {
                      callSuper(name=name,db=db,
                                messSet=messSet,
                                ...)
                    },
                  messdb = function () {
                    db
                  },
                  receiveMessage = function (mess) {
                    stop(sprintf("Abstract Listener %s has no receive message method.",name))
                  },
                  reset = function(app) {
                    stop(sprintf("Abstract Listener %s has no reset message method.",name))
                  },
                  listeningFor = function(newSet) {
                    if (!missing(newSet))
                      messSet <<- newSet
                    messSet
                  }
              ))


setMethod("isListener","RefListener",function(x) TRUE)
setMethod("receiveMessage","RefListener",
          function(x,message) {
            if (length(listeningFor(x))==0L || mess(message) %in% listeningFor(x)) {
              x$receiveMessage(message)
            } else {
              flog.debug("Listener %s ignoring message %s",listenerName(x),toString(message))
            }
})
setMethod("listenerName","RefListener",function(x) x$name)
setGeneric("clearMessages",function(x,app) standardGeneric("clearMessages"))
setMethod("clearMessages","RefListener",function(x,app) {x$reset(app); x})
setMethod("listeningFor", "RefListener", function (x, newSet) {
  if (missing(newSet)) x$listeningFor()
  else x$listeningFor(newSet)
  })


#############################################
## Listener Set

ListenerSet <-
  setRefClass("ListenerSet",
              fields=c(sender="character",
                       listeners="list",
                       db="JSONDB",
                       adminDB="JSONDB"),
              methods = list(
                  initialize =
                    function(sender="sender",listeners=list(),
                             db=mongo::MongoDB(noMongo=TRUE),
                             adminDB=mongo::MongoDB(noMongo=TRUE),
                             ...) {
                      callSuper(sender=sender,
                                listeners=listeners,
                                db=db,
                                adminDB=adminDB,
                                ...)
                    }
              ))


## Listener/Message Methods
ListenerSet$methods(
                admindb = function() adminDB,
                messdb = function() db,
                registerOutput = function (name, filename, app, process,
                                           type="data", doc="") {
                  if (!mdbAvailable(admindb())) return()
                  flog.info("Registering %s file %s.",type,name)
                  newrec <- buildJQuery(
                      app=app,process=process,type=type,
                      name=name,filename=filename,
                      timestamp=as.character(Sys.time()),
                      doc=doc)
                  qq <- buildJQuery(app=app,name=name, process=process)
                  if (mdbCount(admindb(),qq) == 0L)
                    mdbInsert(admindb(),newrec)
                  else
                    mdbUpdate(admindb(),qq,sprintf('{"$set":%s}',newrec))
                }
            )

ListenerSet$methods(
                addListener = function (name,listener) {
                  if (!isListener(listener)) {
                    stop(listener,"is not a listener.")
                  }
                  listeners[[name]] <<- listener
                },
                removeListener = function (name) {
                  listeners[[name]] <<- NULL
                },
                notifyListeners = function (messge) {
                  sender(messge) <- sender
                  message <- saveRec(messdb(),messge)
                  flog.info("Sending message %s",toString(messge))
                  flog.info(".. from %s",sender)
                  flog.debug("Message:",x=as.jlist(messge,attributes(messge)),
                             capture=TRUE)
                  for (name in names(listeners)) {
                    flog.info(".... to %s",name)
                    receiveMessage(listeners[[name]],messge)
                  }
                },
                reset = function(app) {
                  mdbRemove(messdb(),buildJQuery(app=app))
                }
            )

ListenerSet <- function(sender="Proc4",listeners=list(),
                        db=mongo::MongoDB(noMongo=TRUE),
                        adminDB=mongo::MongoDB(noMongo=TRUE)) {
  new("ListenerSet",sender=sender,listeners=listeners,
      db=db, adminDB=adminDB)
}
setMethod("receiveMessage","ListenerSet",
          function(x,message) x$notifyListeners(message))
setMethod("notifyListeners","ListenerSet",
          function(sender,message) sender$notifyListeners(message))
setMethod("isListener","ListenerSet",
          function(x) TRUE)
setMethod("isListener","ANY",
          function(x) hasMethod("receiveMessage",class(x)))
setMethod("clearMessages","ListenerSet", function(x, app) {x$reset(app); x})
setMethod("resetListeners","ListenerSet", function(x,which,app) {
  if (any(which=="ALL") || "Self" %in% which)
    clearMessages(x,app)
  for (name in names(x$listeners))
    if (any(which=="ALL") || name %in% which)
      clearMessages(x$listeners[[name]],app)
  x
})


## Fields we may need to deal with:
## name
## type
## dbname
## dburi
## colname
## mongoverbose
## ssl_options
## qfields -- Upsert/Update Only
## targetField -- Update Only
## fieldlist -- Table only
## messSet
buildListener <- function (specs,app,dburi,defaultdb="Proc4",
                           ssl_options=mongolite::ssl_options(),
                           noMongo=!missing(dburi)&&length(dburi)>0L&&nchar(dburi)>0L) {
  name <- gsub("<app>",basename(app),as.character(specs$name),fixed=TRUE)
  type <- specs$type
  class <- findClass(type)
  if (length(class)==0)
    stop("Cannot find class ",type, "for listener ",name)
  args <- list(name=name)

  ## Substitute for <app> in sender field
  if (!is.null(specs$sender)) {
    args <- c(args,
              sender = list(gsub("<app>",app,as.character(specs$sender),fixed=TRUE)))
  }
  ## dburi and ssl_options are set by the caller, not the config.json
  dbname <- as.character(specs$dbname)[1]
  if (is.null(dbname)) dbname <- defaultdb
  colname <- as.character(specs$colname)[1]
  if (is.null(colname)) colname <- paste(name,"Messages",sep="")
  mongoverbose <- as.logical(specs$mongoverbose)[1]
  if (is.null(mongoverbose) || is.na(mongoverbose) || length(mongoverbose)==0L)
    mongoverbose <- length(dburi)>0L && nchar(dburi)
  ## Note name change here.
  messDB <- mongo::MongoDB(colname,dbname,dburi,
                           verbose=mongoverbose,
                           noMongo=noMongo,
                           options=ssl_options)
  args <- c(args, db=messDB)
  ##
  if (!is.null(specs$messages)) {
    args <- c(args, messSet = list(as.character(specs$messages)))
  }
  if (!is.null(specs$targetField)) {
    args <- c(args, targetField = list(as.character(specs$targetField)))
  }
  if (!is.null(specs$jsonEncoder)) {
    args <- c(args, jsonEncoder = list(as.character(specs$jsonEncoder)))
  }
  ## qfields
  if (!is.null(specs$qfields)) {
    args <- c(args, qfields = list(as.character(specs$qfields)))
  }
  ## feildList
  if (!is.null(specs$fields)) {
    fieldlist <- as.character(specs$fields)
    names(fieldlist) <- names(specs$fields)
    args <- c(args,fieldlist=list(fieldlist))
  }

  flog.info("Building %s with name %s.\n",name,type)
  flog.info("Args:",args,capture=TRUE)
  do.call(type,args)

}

## Used in both EA and EI.
setClassUnion("NullListenerSet",c("ListenerSet","NULL"))
