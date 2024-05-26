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

setGeneric("listenerDataTable",function(listener,appid=character())
  standardGeneric("listenerDataTable"))



#############################################
## Abstract Listener

RefListener <-
  setRefClass("RefListener",
              fields=c(name = "character",
                       messSet = "character",
                       db="MongoDB"
                      ),
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
                  receiveMessage = function (message) {
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

setMethod("listenerDataTable","RefListener",
          function(listener,appid=character())
            NULL)


#############################################
## Listener Set

ListenerSet <-
  setRefClass("ListenerSet",
              fields=c(sender="character",
                       listeners="list",
                       db="JSONDB",
                       registryDB="JSONDB"),
              methods = list(
                  initialize =
                    function(sender="sender",listeners=list(),
                             db=mongo::MongoDB(noMongo=TRUE),
                             registryDB=mongo::MongoDB(noMongo=TRUE),
                             ...) {
                      callSuper(sender=sender,
                                listeners=listeners,
                                db=db,
                                registryDB=registryDB,
                                ...)
                    }
              ))


## Listener/Message Methods
ListenerSet$methods(
                registrydb = function() registryDB,
                messdb = function() db,
                registerOutput = function (name, filename, app, process,
                                           type="data", doc="") {
                  if (!mdbAvailable(registrydb())) return()
                  flog.info("Registering %s file %s.",type,name)
                  newrec <- buildJQuery(
                      app=app,process=process,type=type,
                      name=name,filename=filename,
                      timestamp=as.character(Sys.time()),
                      doc=doc)
                  qq <- buildJQuery(app=app,name=name, process=process)
                  if (mdbCount(registrydb(),qq) == 0L)
                    mdbInsert(registrydb(),newrec)
                  else
                    mdbUpdate(registrydb(),qq,sprintf('{"$set":%s}',newrec))
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
                        registryDB=mongo::MongoDB(noMongo=TRUE)) {
  new("ListenerSet",sender=sender,listeners=listeners,
      db=db, registryDB=registryDB)
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
setGeneric("registerOutput",
           function (registrar, name, filename, app, process,
                     type="data", doc="")
             StandardGeneric("registerOutput"))
setMethod("registerOutput","ListenerSet",
          function (registrar, name, filename, app, process,
                    type="data", doc="")
            registrar$registerOutput(name, filename,
                                     app, process,
                                     type=type, doc=doc))

updateTable <- function(ls, which, type="data", appid, outdir,
                        fname="<app>_<name>.csv",
                        process=ls$sender,
                        flattener=jsonlite::flatten,
                        doc="",name=which) {
  sappid <- basename(appid)
  lis <- ls$listners[[which]]
  dat <- listenerDataTable(lis,appid=appid)
  if (!is.null(dat)) {
    if (!is.null(flattener))
      dat <- do.call(flattener,list(dat))
    fname <- gsub("<app>",sappid,
                  gsub("<name>",name,fname))
    utils::write.csv(dat,file.path(outdir,fname))
    ls$registerOutput(name, fname,file.path(outdir,fname),
                      appid=appid,process=process,doc=doc)
  }
  invisible(dat)
}

generateListenerExports <- function(ls, exportlist, appid, outdir,
                                    process=ls$sender) {
  for (config in exportlist) {
    do.call(updateTable,c(list(appid=appid, outdir=outdir, process=process),
                          config))
  }
}

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
buildListener <- function (specs,app,dburi,defaultDB="Proc4",
                           ssl_options=mongolite::ssl_options(),
                           noMongo=!missing(dburi)&&length(dburi)>0L&&nchar(dburi)>0L) {
  name <- gsub("<app>",basename(app),as.character(specs$name),fixed=TRUE)
  type <- specs$type
  flog.info("Building %s with type %s.\n",name,type)
  flog.debug("Specs=",specs,capture=TRUE)
  class <- findClass(type)
  if (length(class)==0L)
    stop("Cannot find class ",type, "for listener ",name)
  args <- list(name=name)

  ## Substitute for <app> in sender field
  if (!is.null(specs$sender)) {
    args <- c(args,
              sender = list(gsub("<app>",app,as.character(specs$sender),fixed=TRUE)))
  }
  ## dburi and ssl_options are set by the caller, not the config.json
  dbname <- as.character(specs$dbname)[1]
  flog.trace("dbname=",dbname,capture=TRUE)
  if (is.null(dbname)) dbname <- defaultDB
  colname <- as.character(specs$colname)[1]
  flog.trace("colname=",colname,capture=TRUE)
  if (is.null(colname)) colname <- paste(name,"Messages",sep="")
  mongoverbose <- as.logical(specs$mongoverbose)[1]
  flog.trace("mongoverbose",mongoverbose,capture=TRUE)
  flog.trace("dburi",dburi,capture=TRUE)
  flog.trace("length(dburi)",length(dburi),capture=TRUE)
  flog.trace("nchar(dburi)",nchar(dburi),capture=TRUE)

  if (is.null(mongoverbose) || is.na(mongoverbose) || length(mongoverbose)==0L)
    mongoverbose <- length(dburi)>0L && nchar(dburi) >0L
  ## Note name change here.

  flog.trace("Connecting to database %s:%s:%s, verbose=%s, noMongo=%s",
             dburi,dbname,colname,mongoverbose,noMongo)
  flog.trace("SSL options: ",ssl_options,capture=TRUE)
  messDB <- mongo::MongoDB(colname,dbname,dburi,
                           verbose=mongoverbose,
                           noMongo=noMongo,
                           options=ssl_options)
  args <- c(args, db=messDB)
  flog.trace("Args:",args,capture=TRUE)
  ##
  flog.trace("messages=",specs$messages,capture=TRUE)
  if (!is.null(specs$messages)) {
    args <- c(args, messSet = list(as.character(specs$messages)))
  }
  flog.trace("targetField=",specs$targetField,capture=TRUE)
  if (!is.null(specs$targetField)) {
    args <- c(args, targetField = list(as.character(specs$targetField)))
  }
  flog.trace("jsonEncoder=",specs$jsonEncoder,capture=TRUE)
  if (!is.null(specs$jsonEncoder)) {
    args <- c(args, jsonEncoder = list(as.character(specs$jsonEncoder)))
  }
  ## qfields
  flog.trace("qfields=",specs$qfields,capture=TRUE)
  if (!is.null(specs$qfields)) {
    args <- c(args, qfields = list(as.character(specs$qfields)))
  }
  ## feildList
  flog.trace("fields=",specs$fields,capture=TRUE)
  if (!is.null(specs$fields)) {
    fieldlist <- as.character(specs$fields)
    names(fieldlist) <- names(specs$fields)
    args <- c(args,fieldlist=list(fieldlist))
  }

  flog.info("Args:",args,capture=TRUE)
  do.call(type,args)

}

buildListenerSet <- function(sender,config,appid,
                             lscol,dbname,dburi,sslops,
                             registrycol,registrydbname,
                             mongoverbose=FALSE) {
  listeners <- lapply(config,buildListener,appid,
                      dburi,dbname,sslops)
  names(listeners) <- as.character(sapply(config,
                                          function (lc) {
                                            lc$name
                                            }))
  flog.info("Building Listener Set for %s.\n",sender)
  ListenerSet(sender=sender,
              db=mongo::MongoDB(lscol,dbname,dburi,verbose=mongoverbose,
                                options=sslops),
              registryDB=mongo::MongoDB(registrycol,registrydbname,
                                        dburi,verbose=mongoverbose,
                                        options=sslops),
              listeners=listeners)
}


## Used in both EA and EI.
setClassUnion("NullListenerSet",c("ListenerSet","NULL"))
