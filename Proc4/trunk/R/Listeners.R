## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.

setGeneric("receiveMessage",function(x,mess) standardGeneric("receiveMessage"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setMethod("isListener","ANY",function(x) FALSE)
setGeneric("notifyListeners",function(sender,mess)
  standardGeneric("notifyListeners"))
#setClass("Listener",contains="VIRTUAL")
#setMethod("isListener","Listener",function(x) TRUE)


setOldClass("mongo")
setClassUnion("MongoDB",c("mongo","NULL"))

#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.

CaptureListener <-
  setRefClass("CaptureListener",
              fields=c(messages="list"),
              methods=list(
                  initialize = function(messages=list(),...)
                    callSuper(messages=messages,...),
                  receiveMessage = function (mess) {
                    messages <<- c(mess,messages)
                  },
                  lastMessage = function() {
                    messages[[1]]
                  }))

CaptureListener <- function (messages=list(),...) {
  new("CaptureListener",messages=messages,...)
}

setMethod("isListener","CaptureListener",function(x) TRUE)
setMethod("receiveMessage","CaptureListener",
          function(x,mess) x$receiveMessage(mess))


#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

InjectionListener <-
  setRefClass("InjectionListener",
              fields=c(sender="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             ...) {
                      callSuper(sender=sender,db=NULL,
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
                  }
              ))

InjectionListener <- function (sender="sender",
                               dbname="test",
                               dburi="mongodb://localhost",
                               messSet=character(),
                               colname="Messages",...) {
  new("InjectionListener",sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,messSet=messSet,...)
}

setMethod("isListener","InjectionListener",function(x) TRUE)
setMethod("receiveMessage","InjectionListener",
          function(x,mess) x$receiveMessage(mess))

#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

UpsertListener <-
  setRefClass("UpsertListener",
              fields=c(sender="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       qfields="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(sender=sender,db=NULL,
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
                  }
              ))

UpsertListener <- function (sender="sender",
                            dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            qfields=c("app","uid"),...) {
  new("UpsertListener",sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,messSet=messSet,qfields=qfields,...)
}

setMethod("isListener","UpsertListener",function(x) TRUE)
setMethod("receiveMessage","UpsertListener",
          function(x,mess) x$receiveMessage(mess))


#################################################
## UpdateListener

## This listener updates fields in a reference database for a given
## listener.  It can be used, for example, to keep track of trophies
## earned or balance of money/earned or spent in the game.

UpdateListener <-
  setRefClass("UpdateListener",
              fields=c(dbname="character",
                       dburi="character",
                       colname="character",
                       messSet = "character",
                       qfields="character",
                       targetField="character",
                       jsonEncoder="character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             targetField="data",
                             jsonEncoder="unparseData",
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(db=NULL,
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
                  }
              ))

UpdateListener <- function (dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            targetField="data",
                            qfields=c("app","uid"),
                            jsonEncoder="unparseData",
                            ...) {
  new("UpdateListener",dbname=dbname,dburi=dburi,messSet=messSet,
      colname=colname,targetField=targetField,jsonEncoder=jsonEncoder,
      qfields=qfields,...)
}

setMethod("isListener","UpdateListener",function(x) TRUE)
setMethod("receiveMessage","UpdateListener",
          function(x,mess) x$receiveMessage(mess))


#############################################
## Listener Set

ListenerSet <-
  setRefClass("ListenerSet",
              fields=c(sender="character",
                       dbname="character",
                       dburi="character",
                       colname="character",
                       listeners="list",
                       db="MongoDB"),
              methods = list(
                  initialize =
                    function(sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             listeners=list(),colname="Messages",
                             ...) {
                      callSuper(sender=sender,db=NULL,
                                dburi=dburi,dbname=dbname,
                                colname=colname,listeners=listeners,
                                ...)
                    }
              ))


## Listener/Message Methods
ListenerSet$methods(
                messdb = function () {
                  if (is.null(db)) {
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

