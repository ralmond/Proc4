## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.

setGeneric("receiveMessage",function(x,mess) standardGeneric("receiveMessage"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setGeneric("resetListeners",function(x,which,app) standardGeneric("resetListeners"))
setMethod("isListener","ANY",function(x) FALSE)
setGeneric("notifyListeners",function(sender,mess)
  standardGeneric("notifyListeners"))
#setClass("Listener",contains="VIRTUAL")
#setMethod("isListener","Listener",function(x) TRUE)
setGeneric("listenerName", function (x) standardGeneric("listenerName"))


setOldClass("mongo")
setClassUnion("MongoDB",c("mongo","NULL"))

#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.

CaptureListener <-
  setRefClass("CaptureListener",
              fields=c(name = "character",messages="list"),
              methods=list(
                  initialize = function(name="Capture",messages=list(),...)
                    callSuper(name=name,messages=messages,...),
                  receiveMessage = function (mess) {
                    messages <<- c(mess,messages)
                  },
                  lastMessage = function() {
                    messages[[1]]
                  },
                  reset = function(app) {
                    messages <<- list()
                  }))

CaptureListener <- function (name="Capture",messages=list(),...) {
  new("CaptureListener",name=name,messages=messages,...)
}

setMethod("isListener","CaptureListener",function(x) TRUE)
setMethod("receiveMessage","CaptureListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","CaptureListener",function(x) x$name)



#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

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
                      db <<- mongo(colname,dbname,dburi)
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

#################################################
## UpsertListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

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
                      db <<- mongo(colname,dbname,dburi)
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


#################################################
## UpdateListener

## This listener updates fields in a reference database for a given
## listener.  It can be used, for example, to keep track of trophies
## earned or balance of money/earned or spent in the game.

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
                      db <<- mongo(colname,dbname,dburi)
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

##############################################
## Table Listener -- Based on work by Lukas Liu and Nan Wang

TableListener <-
  setRefClass("TableListener",
              fields=list(name = "character",
                          fieldlist = "character",  #Named list of types
                          df = "data.frame",
                          messSet="character"
                          ),
              methods=list(
                  initialize =
                    function(name="ppData",
                             fieldlist=c("uid"="character",
                                         "context"="character"),
                             messSet=character(),
                             ...){
                      callSuper(name=name,
                                fieldlist=fieldlist,
                                messSet=messSet,
                                df=data.frame(),...)
                },
                initDF = function(){
                  ##initializes the data frame with proper dimensions
                  ##(uid, app, and context and mandatory)
                  fields <- vector("list",length(fieldlist))
                  names(fields) <- names(fieldlist)
                  for (f in names(fields)) {
                    if (grepl("ordered",fieldlist[f])) {
                      levels <-  strsplit(sub("ordered\\((.*)\\)","\\1",
                                              fieldlist[f]),",")[[1]]
                      fields[[f]] <- ordered(NA,levels)
                    } else if (grepl("factor",fieldlist[f])) {
                      levels <- strsplit(sub("factor\\((.*)\\)","\\1",
                                             fieldlist[f]),",")[[1]]
                      fields[[f]] <- factor(NA,levels)
                    } else if (fieldlist[f]=="difftime") {
                      fields[[f]] <- as.difftime(NA_real_,units="secs")
                    } else {
                      fields[[f]] <- vector(fieldlist[f],1)
                      fields[[f]][1] <-NA
                    }
                  }
                  df <<- do.call("data.frame",c(fields,stringsAsFactors=FALSE))
                  flog.trace("First row:",df,capture=TRUE)
                },
                receiveMessage = function(mess){
                  ## captures the incoming message and stick into the
                  ## data frame.
                  if (ncol(df)==0L) initDF()
                  if (mess(mess) %in% messSet) {
                    flog.debug("Adding row for %s (%s): %s",uid(mess),
                               context(mess), toString(mess))
                    new.line <- df[1,,drop=FALSE]
                    for (f in names(new.line)) {
                      new.val <- switch(f,
                                        "app"=app(mess),
                                        "context"=context(mess),
                                        "uid"=uid(mess),
                                        "mess"=mess(mess),
                                        "sender"=sender(mess),
                                        "timestamp"=ifelse(typeof(new.line$timestamp) == "character",
                                                           as.character(timestamp(mess)),
                                                           timestamp(mess)),
                                        details(mess)[[f]])
                      if(!is.null(new.val)) new.line[1,f] <- new.val
                    }
                    df <<- rbind(df, new.line)
                  }
                },
                reset = function(app) {
                    initDF()
                },
                returnDF = function() {
                  ## deletes the first row of DF because initDF()
                  ## creates an empty row
                  df[-1, ]
                }
                ))


TableListener <- function (name="ppData",
                             fieldlist=c("uid"="character",
                                         "context"="character"),
                             messSet=character(),...) {
  new("TableListener",name=name,fieldlist=fieldlist,messSet=messSet,...)
}

setMethod("isListener","TableListener",function(x) TRUE)
setMethod("receiveMessage","TableListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","TableListener",function(x) x$name)


#############################################
## Listener Set

ListenerSet <-
  setRefClass("ListenerSet",
              fields=c(sender="character",
                       dbname="character",
                       admindbname="character",
                       dburi="character",
                       colname="character",
                       listeners="list",
                       db="MongoDB",
                       adminDB="MongoDB"),
              methods = list(
                  initialize =
                    function(sender="sender",
                             dbname="test",
                             admindbname="",
                             dburi="mongodb://localhost",
                             listeners=list(),
                             colname="Messages",
                             ...) {
                      callSuper(sender=sender,db=NULL,
                                dburi=dburi,dbname=dbname,
                                admindbname=admindbname,
                                colname=colname,listeners=listeners,
                                ...)
                    }
              ))


## Listener/Message Methods
ListenerSet$methods(
                admindb = function () {
                  if (is.null(adminDB) && nchar(dburi) > 0L
                      && nchar(admindbname) > 0L) {
                    adminDB <<- mongo("OutputFiles",admindbname,dburi)
                  }
                  adminDB
                },
                registerOutput = function (name, filename, app, process,
                                           type="data", doc="") {
                  if (!is.null(admindb())) {
                    flog.info("Registering %s file %s.",type,name)
                    newrec <- buildJQuery(
                        app=app,process=process,type=type,
                        name=name,filename=filename,
                        timestamp=as.character(Sys.time()),
                        doc=doc)
                    qq <- buildJQuery(app=app,name=name, process=process)
                    if (admindb()$count(qq) == 0L)
                      admindb()$insert(newrec)
                    else
                      admindb()$update(qq,sprintf('{"$set":%s}',newrec))
                  }
                }

            )

ListenerSet$methods(
               messdb = function () {
                  if (is.null(db) && nchar(dburi) > 0L) {
                    db <<- mongo(colname,dbname,dburi)
                  }
                  db
                },
                addListener = function (name,listener) {
                  if (!isListener(listener)) {
                    stop(listener,"is not a listener.")
                  }
                  listeners[[name]] <<- listener
                },
                removeListener = function (name) {
                  listeners[[name]] <<- NULL
                },
                notifyListeners = function (mess) {
                  mess <- saveRec(mess,messdb())
                  flog.info("Sending message %s",toString(mess))
                  flog.info(".. from %s",sender)
                  flog.debug("Message:",x=as.jlist(mess,attributes(mess)),
                             capture=TRUE)
                  for (name in names(listeners)) {
                    flog.info(".... to %s",name)
                    receiveMessage(listeners[[name]],mess)
                  }
                },
                reset = function(app) {
                  if (!is.null(messdb()))
                    messdb()$remove(buildJQuery(app=app))
                }
            )


setMethod(receiveMessage,"ListenerSet",
          function(x,mess) x$notifyListeners(mess))
setMethod(notifyListeners,"ListenerSet",
          function(sender,mess) sender$notifyListeners(mess))
setMethod(isListener,"ListenerSet",
          function(x) TRUE)
setMethod(isListener,"ANY",
          function(x) hasMethod("receiveMessage",class(x)))
setMethod("resetListeners","ListenerSet", function(x,which,app) {
  if (which=="ALL" || "Self" %in% which)
    x$reset(app)
  for (name in names(x$listeners))
    if (which=="ALL" || name %in% which)
      x$listeners[[name]]$reset(app)
  x
})


## Fields we may need to deal with:
## name
## type
## dbname
## dburi
## colname
## qfields -- Upsert/Update Only
## targetField -- Update Only
## fieldlist -- Table only
## messSet
buildListener <- function (specs,app,dburi) {
  name <- gsub("<app>",app,as.character(specs$name),fixed=TRUE)
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
  ## dburi is set by the caller, not the config.json
  if (!is.null(specs$dburi)) {
    args <- c(args, dburi =list(dburi))
  }
  if (!is.null(specs$dbname)) {
    args <- c(args, dbname = list(as.character(specs$dbname)))
  }
  if (!is.null(specs$colname)) {
    args <- c(args, colname = list(as.character(specs$colname)))
  }
  ## Note name change here.
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
