## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.

setGeneric("receiveMessage",function(x,mess) standardGeneric("receiveMessage"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setMethod("isListener","ANY",function(x) FALSE)
setGeneric("notifyListeners",function(sender,mess)
  standardGeneric("notifyListeners"))

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
                      flog.info("Sending message %s",toString(mess))
                      flog.info(".. from %s",sender)
                      flog.debug("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mess@sender <- sender
                      mess <- saveRec(mess,messdb())
                    } else {
                      flog.debug("%s ignoring message %s",sender,toString(mess))
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
                             ...) {
                      callSuper(db=NULL,
                                dburi=dburi,dbname=dbname,
                                colname=colname,messSet=messSet,
                                targetField=targetField,
                                jsonEncoder=jsonEncoder,
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
                      flog.info("Updating record for %s: %s",uid(mess),toString(mess))
                      flog.debug("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      if (nchar(targetField) > 0L) {
                        update <- sprintf('{"$set":{"%s":%s, "timestamp":%s}}',
                                          targetField,
                                          do.call(jsonEncoder,
                                                  list(details(mess))),
                                          toJSON(unboxer(timestamp(mess)),
                                                 POSIXt="mongo"))
                      } else {
                        update <- sprintf('{"$set":%s}',
                                          do.call(jsonEncoder,
                                                  list(details(mess))))
                      }
                      qq <- buildJQuery(app=app(mess),uid=uid(mess))
                      if (messdb()$count(qq) == 0L) {
                        ## Initializize by saving message.
                        messdb()$insert(as.json(mess))
                      }
                      messdb()$update(qq,update)
                    } else {
                      flog.debug("%s ignoring message %s",sender,toString(mess))
                    }
                  }
              ))

UpdateListener <- function (dbname="test",
                            dburi="mongodb://localhost",
                            colname="Messages",
                            targetField="data",
                            jsonEncoder="unparseData",
                            ...) {
  new("UpdateListener",dbname=dbname,dburi=dburi,
      colname=colname,targetField=targetField,jsonEncoder=jsonEncoder,...)
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

