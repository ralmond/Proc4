## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.



#' A listener is an object which can recieve a message.
#' 
#' 
#' A \emph{listener} an an object that takes on the observer or listerner role
#' in the the listener (or observer) design pattern.  A listener will register
#' itself with a speaker, and when the speaker sends a message it will act
#' accordingly.  The \code{receiveMessage} generic function must be implemented
#' by a listener.  It is called when the speaker wants to send a message.
#' 
#' 
#' The \code{Listener} class is a virtual class.  Any object can become a
#' listener by giving it a method for \code{receiveMessage}.  The message is
#' intended to be a subclass of \code{\linkS4class{P4Message}}, but in
#' practice, no restriction is placed on the type of the message.
#' 
#' As \code{Listener} is a virtual class, it does not have a formal definition.
#' Instead the generic function \code{isListner} is used to test if the object
#' is a proper listener or not.  The default method checks for the presence of
#' a \code{receiveMessage} method.  As this might not work properly with S3
#' objects, an object can also register itself directly by setting a method for
#' \code{isListner} which returns true.
#' 
#' Typically, a lister will register itself with the speaker objects.  For
#' example the \code{\linkS4class{ListenerSet}$addListener} method adds itself
#' to a list of listeners maintained by the object.  When the
#' \code{\linkS4class{ListenerSet}$notifyListeners} method is called, the
#' \code{receiveMessage} method is called on each listener in the list.
#' 
#' @aliases Listener-class Listener receiveMessage isListener
#' isListener,ANY-method
#' @param x A object of the virtual class \code{Listner}.
#' @param mess A \code{\linkS4class{P4Message}} which is being transmitted.
#' @return
#' 
#' The \code{isListener} function should return \code{TRUE} or \code{FALSE},
#' according to whether or not the object follows the listner protocol.
#' 
#' The \code{receiveMessage} function is typically invoked for side effects and
#' it may have any return value.
#' @author Russell Almond
#' @seealso Implementing Classes: \code{\linkS4class{CaptureListener}},
#' \code{\linkS4class{UpdateListener}}, \code{\linkS4class{UpsertListener}},
#' \code{\linkS4class{InjectionListener}}, \code{\linkS4class{TableListener}}
#' 
#' Related Classes: \code{\linkS4class{ListenerSet}},
#' \code{\linkS4class{P4Message}}
#' @references
#' 
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}
#' @keywords interface objects
#' @examples
#' 
#' 
#' \dontrun{## Requires Mongo database set up.
#' MyListener <- setClass("MyListener",slots=c("name"="character"))
#' setMethod("receiveMessage","MyListener",
#'    function(x,mess)
#'       cat("I (",x@name,") just got the message ",mess(mess),"\n"))
#' 
#' 
#' lset <-
#' ListenerSet$new(sender="Other",dburi="mongodb://localhost",
#'                 colname="messages")
#' lset$addListener("me",MyListener())
#' 
#' mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=TRUE,seletion="D"))
#' 
#' mess2 <- P4Message("Fred","Task 2","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:17:25 EST"),
#'          list(correct=FALSE,seletion="D"))
#' 
#' lset$notifyListeners(mess1)
#' 
#' lset$removeListener("me")
#' 
#' notifyListeners(lset,mess2)
#' 
#' }
#' 
setGeneric("receiveMessage",function(x,mess) standardGeneric("receiveMessage"))
setGeneric("isListener",function(x) standardGeneric("isListener"))
setMethod("isListener","ANY",function(x) FALSE)



#' Clears messages caches associated with listeners
#' 
#' 
#' Listeners often cache the messages in some way.  This causes the message
#' cache to be cleared, and operation which is often useful before a rerun.
#' The \code{which} argument is used to control which listeners should have
#' their cache cleared.
#' 
#' 
#' Each \code{\link{Listener}} object (including the listener set) has a
#' \code{$reset()} method which empties the cache of messages.  This method
#' calls the \code{$reset()} method for each of the listeners named in
#' \code{which}.  The special keyword \dQuote{ALL} is used to reset all
#' listeners and the special keyword \dQuote{Self} is used to refer to the
#' \code{\link{ListenerSet}} object itself (which may have a database
#' colleciton).
#' 
#' @aliases resetListeners resetListeners,ListenerSet-method
#' resetListeners,NULL-method
#' @param x A \code{\linkS4class{ListenerSet}} object containing the listeners
#' to be reset.
#' @param which A character vector containing the names of the listeners to
#' reset.  The special keyword \dQuote{ALL} means all listeners will be reset.
#' The special keyword \dQuote{Self} means that the cache associated with the
#' listener set will be reset.
#' @param app A global applicaiton identifier. The reset operation should only
#' be applied to messages from this application.
#' @return The \code{\link{ListenerSet}} object is returned.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{ListenerSet}}, \code{\link{Listener}}
#' @keywords interface database
#' @examples
#' 
#' 
#' \dontrun{## Requires Mongo database set up.
#' 
#' data2json <- function(dat) {
#'   toJSON(sapply(dat,unboxer))
#' }
#' 
#' listeners <- list(
#'   cl = CaptureListener(name="cl"),
#'   upd = UpdateListener(name="upd",messSet="New Observables",
#'                          dburi="mongodb://localhost",dbname="test",
#'                          targetField="data",jsonEncode="data2json",
#'                          colname="Updated"),
#'   ups = UpsertListener(name="ups",sender="EIEvent",messSet="New Observables",
#'                          dburi="mongodb://localhost",dbname="test",
#'                          colname="Upserted", qfields=c("app","uid")),
#'   il = InjectionListener(name="il",sender="EIEvent",messSet="New Observables",
#'                          dburi="mongodb://localhost",dbname="test",
#'                          colname="Injected"),
#'   tl = TableListener(name="tl",
#'                    messSet="New Observables",
#'                    fieldlist=c(uid="character", context="character",
#'                                timestamp="character",
#'                                solvedtime="numeric",
#'                                trophy="ordered(none,silver,gold)"))
#'   )
#'   
#' lset <- ListenerSet$new(sender="Other",dburi="mongodb://localhost",
#'                 colname="messages",dbname="test",listeners=listeners)
#' 
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EIEvent",mess="New Observables",
#'                    details=list(trophy="gold",solvedtime=10))
#' 
#' resetListeners(lset,"ALL","default")
#' receiveMessage(lset,mess1)
#' ## Check recieved messages.
#' stopifnot(lset$messdb()$count(buildJQuery(app="default"))==1L,
#'           length(listeners$cl$messages)==1L,
#'           listeners$upd$messdb()$count(buildJQuery(app="default"))==1L,
#'           listeners$ups$messdb()$count(buildJQuery(app="default"))==1L,
#'           listeners$il$messdb()$count(buildJQuery(app="default"))==1L,
#'           nrow(listeners$tl$returnDF())==1L)
#' 
#' resetListeners(lset,c("Self","cl","il","tl"),"default")
#' stopifnot(lset$messdb()$count(buildJQuery(app="default"))==0L,
#'           length(listeners$cl$messages)==0L,
#'           listeners$upd$messdb()$count(buildJQuery(app="default"))==1L,
#'           listeners$ups$messdb()$count(buildJQuery(app="default"))==1L,
#'           listeners$il$messdb()$count(buildJQuery(app="default"))==0L,
#'           nrow(listeners$tl$returnDF())==0L)
#' 
#' resetListeners(lset,"ALL","default")
#' stopifnot(lset$messdb()$count(buildJQuery(app="default"))==0L,
#'           length(listeners$cl$messages)==0L,
#'           listeners$upd$messdb()$count(buildJQuery(app="default"))==0L,
#'           listeners$ups$messdb()$count(buildJQuery(app="default"))==0L,
#'           listeners$il$messdb()$count(buildJQuery(app="default"))==0L,
#'           nrow(listeners$tl$returnDF())==0L)
#' 
#' 
#' }
#' 
setGeneric("resetListeners",function(x,which,app) standardGeneric("resetListeners"))
setMethod("resetListeners","NULL",function(x,which,app) x)

#' Notifies listeners that a new message is available.
#' 
#' 
#' This is a generic function for objects that send \code{\link{P4Message}}
#' objects.  When this function is called, the message is sent to the
#' listeners; that is, the \code{\link{receiveMessage}} function is called on
#' the listener objects.  Often, this protocol is implemented by having the
#' \code{sender} include a \code{\link{ListenerSet}} object.
#' 
#' 
#' @param sender An object which sends messages.
#' @param mess A \code{\link{P4Message}} to be sent.
#' @return
#' 
#' Function is invoked for its side effect, so return value may be anything.
#' @author Russell Almond
#' @seealso \code{\link{P4Message}}, \code{\linkS4class{Listener}},
#' \code{\link{ListenerSet}}
#' @keywords interface objects
#' @examples
#' 
#' \dontrun{## Requires Mongo database set up.
#' MyListener <- setClass("MyListener",slots=c("name"="character"))
#' setMethod("receiveMessage","MyListener",
#'    function(x,mess)
#'       cat("I (",x@name,") just got the message ",mess(mess),"\n"))
#' 
#' 
#' lset <-
#' ListenerSet$new(sender="Other",dburi="mongodb://localhost",
#'                 colname="messages")
#' lset$addListener("me",MyListener())
#' 
#' mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=TRUE,seletion="D"))
#' 
#' mess2 <- P4Message("Fred","Task 2","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:17:25 EST"),
#'          list(correct=FALSE,seletion="D"))
#' 
#' lset$notifyListeners(mess1)
#' 
#' lset$removeListener("me")
#' 
#' notifyListeners(lset,mess2)
#' 
#' }
#' 
#' @export notifyListeners
setGeneric("notifyListeners",function(sender,mess)
  standardGeneric("notifyListeners"))
#setClass("Listener",contains="VIRTUAL")
#setMethod("isListener","Listener",function(x) TRUE)



#' Returns the name of an object.
#' 
#' 
#' This returns a character string identifying the object.
#' 
#' 
#' @param x An object whose name is of interest.
#' @return
#' 
#' A character scalar identifying the object.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\linkS4class{TableListener}}
#' @keywords manip
#' @examples
#' 
#' 
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EIEvent",mess="New Observables",
#'                    details=list(trophy="gold",solvedtime=10))
#' tabMaker <- TableListener(name="Trophy Table",
#'                    messSet="New Observables",
#'                    fieldlist=c(uid="character", context="character",
#'                                timestamp="character",
#'                                solvedtime="numeric",
#'                                trophy="ordered(none,silver,gold)"))
#' 
#' listenerName(tabMaker)
#' 
#' 
setGeneric("listenerName", function (x) standardGeneric("listenerName"))


#' Class \code{"MongoDB"}
#' 
#' 
#' An S4-style class for the \code{\link[mongolite]{mongo}} class.  Note that
#' this is actually a class union, allowing for \code{NULL} if the database is
#' not yet initialized.
#' 
#' 
#' @name MongoDB-class
#' @aliases MongoDB-class MongoDB
#' @docType class
#' @note
#' 
#' The original \code{\link[mongolite]{mongo}} class is an S3 class.  Rather
#' than just call \code{\link[methods]{setOldClass}} and exposing that, I've
#' explosed a class union (\code{\link[methods]{setClassUnion}}) with the
#' \code{mongo} class and \code{NULL}.
#' 
#' A typical usage would have this type used in the slot of an object, which
#' would initialize the value to \code{NULL}, and then set it to a \code{mongo}
#' object when the database connection is openned.
#' @section Objects from the Class:
#' 
#' \code{NULL} is an object of this class.
#' 
#' Objects of this class can be created with calls to
#' \code{\link[mongolite]{mongo}}.
#' @author Russell Almond
#' @seealso \code{\link{ListenerSet}}, \code{\link[mongolite]{mongo}}
#' @keywords classes
#' @examples
#' 
#' showClass("MongoDB")
#' showClass("ListenerSet")
#' lset <- ListenerSet$new()
#' lset$messdb
#' 
setOldClass("mongo")
setClassUnion("MongoDB",c("mongo","NULL"))

#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.
#' Class \code{"CaptureListener"}
#' 
#' 
#' This listener simply takes its messages and adds them to a list.  It is is
#' mainly used for testing the message system.
#' 
#' 
#' This listener simply takes all messages and pushes them onto the
#' \code{messages} field.  The \code{messages} field is the complete list of
#' received messages, most recent to most ancient.  The method
#' \code{lastMessage()} returns the most recent message.
#' 
#' @name CaptureListener-class
#' @aliases CaptureListener-class isListener,CaptureListener-method
#' receiveMessage,CaptureListener-method listenerName,CaptureListener-method
#' @docType class
#' @section Extends:
#' 
#' This class implements the \code{\link{Listener}} interface.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\link{CaptureListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{UpsertListener}}, \code{\linkS4class{InjectionListener}},
#' \code{\linkS4class{TableListener}},
#' @references
#' 
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#' 
#' 
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EABN",mess="Statistics",
#'                    details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
#' 
#' cl <- CaptureListener()
#' receiveMessage(cl,mess1)
#' stopifnot(all.equal(mess1,cl$lastMessage()))
#' 
#' 
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

##############################################
## Table Listener -- Based on work by Lukas Liu and Nan Wang

#' Class \code{"TableListener"}
#' 
#' 
#' A listener that captures data from a \code{\linkS4class{P4Message}} and puts
#' it into a dataframe.
#' 
#' 
#' This listener builds up a data frame with selected data from the messages.
#' What data is captured is controlled by the \code{fieldlist} object.  This is
#' a named character vector whose names correspond to field names and whose
#' values correspond to type names (see \code{\link[base]{typeof}}.  The type
#' can also be one of the two special types, \code{ordered} or \code{factor}.
#' The following is a summary of the most common types: \describe{
#' \item{list("\"numeric\"")}{These are numeric values.}\item{, }{These are
#' numeric values.}\item{list("\"logical\"")}{These are numeric values.}\item{,
#' }{These are numeric values.}\item{list("\"integer\"")}{These are numeric
#' values.}\item{, }{These are numeric values.}\item{ }{These are numeric
#' values.}\item{list("\"double\"")}{These are numeric values.}\item{:}{These
#' are numeric values.} \item{list("\"character\"")}{These are character
#' values.  They are not converted to factors (see factor types
#' below).}\item{:}{These are character values.  They are not converted to
#' factors (see factor types below).} \item{list("\"list\"")}{These are
#' usuable, but should be used with caution because the output data frame may
#' not be easy to export to other program.}\item{,}{These are usuable, but
#' should be used with caution because the output data frame may not be easy to
#' export to other program.}\item{list("\"raw\"")}{These are usuable, but
#' should be used with caution because the output data frame may not be easy to
#' export to other program.}\item{, other values returned by }{These are
#' usuable, but should be used with caution because the output data frame may
#' not be easy to export to other program.}\item{ }{These are usuable, but
#' should be used with caution because the output data frame may not be easy to
#' export to other program.}\item{list(list("typeof"))}{These are usuable, but
#' should be used with caution because the output data frame may not be easy to
#' export to other program.}\item{:}{These are usuable, but should be used with
#' caution because the output data frame may not be easy to export to other
#' program.} \item{list("\"ordered(...)\"")}{These produce objects of type
#' \code{\link[base]{ordered}} and \code{\link[base]{factor}} with the comma
#' separated values between the parenthesis passed as the \code{levels}
#' argument.  For example, \code{"ordered(Low,Medium,High)"} will produces an
#' ordered factor with three levels.  (Note that levels should be in increasing
#' order for ordered factors, but this doesn't matter for unordered
#' factors.)}\item{, }{These produce objects of type
#' \code{\link[base]{ordered}} and \code{\link[base]{factor}} with the comma
#' separated values between the parenthesis passed as the \code{levels}
#' argument.  For example, \code{"ordered(Low,Medium,High)"} will produces an
#' ordered factor with three levels.  (Note that levels should be in increasing
#' order for ordered factors, but this doesn't matter for unordered
#' factors.)}\item{list("\"factor(...)\":")}{These produce objects of type
#' \code{\link[base]{ordered}} and \code{\link[base]{factor}} with the comma
#' separated values between the parenthesis passed as the \code{levels}
#' argument.  For example, \code{"ordered(Low,Medium,High)"} will produces an
#' ordered factor with three levels.  (Note that levels should be in increasing
#' order for ordered factors, but this doesn't matter for unordered factors.)}
#' }
#' 
#' For most fields, the field name is matched to the corresponding element of
#' the \code{\link{details}} of the messages.  The exceptions are the names
#' \code{\link{app}}, \code{\link{context}}, \code{\link{uid}},
#' \code{\link{mess}}, \code{\link{sender}}, \code{\link{timestamp}}, which
#' return the value of the corresponding header fields of the message.  Note
#' that
#' 
#' @name TableListener-class
#' @aliases TableListener-class isListener,TableListener-method
#' receiveMessage,TableListener-method listenerName,TableListener-method
#' @docType class
#' @section Extends:
#' 
#' This class implements the \code{\link{Listener}} interface.
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond, Lukas Liu, Nan Wang
#' @seealso
#' 
#' \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\linkS4class{UpdateListener}}, \code{\linkS4class{InjectionListener}},
#' \code{\linkS4class{CaptureListener}}, \code{\linkS4class{UpsertListener}},
#' \code{\link{TableListener}},
#' @references This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#' 
#' 
#' 
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EIEvent",mess="New Observables",
#'                    details=list(trophy="gold",solvedtime=10))
#' tabMaker <- TableListener(name="Trophy Table",
#'                    messSet="New Observables",
#'                    fieldlist=c(uid="character", context="character",
#'                                timestamp="character",
#'                                solvedtime="numeric",
#'                                trophy="ordered(none,silver,gold)"))
#' 
#' receiveMessage(tabMaker,mess1)
#' tabMaker$returnDF()
#' 
#' 
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

#' Class \code{"ListenerSet"}
#' 
#' 
#' This is a \dQuote{mix-in} class that adds a speaker protocol to an object,
#' which is complementary to the \code{\linkS4class{Listener}} protocol.  This
#' object maintains a list of listeners.  When the \code{notifyListeners}
#' method is called, it notifies each of the listeners by calling the
#' \code{\link{receiveMessage}} method on the listener.
#' 
#' 
#' @name ListenerSet-class
#' @aliases ListenerSet-class ListenerSet NullListenerSet NullListenerSet-class
#' isListener,ListenerSet-method receiveMessage,ListenerSet-method
#' notifyListeners,ListenerSet-method
#' @docType class
#' @note
#' 
#' The \code{notifyListeners} method uses the
#' \code{\link[futile.logger]{flog.logger}} protocol.  In particular, it logs
#' sending the message at the \dQuote{INFO} level, and the actual message sent
#' at the \dQuote{DEBUG} level.  In particular, setting
#' \code{\link[futile.logger]{flog.threshold}(DEBUG,name="Proc4")} will turn on
#' logging of the actual message and
#' \code{\link[futile.logger]{flog.threshold}(WARN,name="Proc4")} will turn off
#' logging of the message sent messages.
#' 
#' It is often useful to redirect the Proc4 logger to a log file.  In addition,
#' changing the logging format to JSON, will allow the message to be recovered.
#' Thus, try
#' \code{\link[futile.logger]{flog.layout}(\link[futile.logger]{layout.json},name="Proc4"}
#' to activate logging in JSON format.
#' @section Extends:
#' 
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.  The class union \code{NullListenerSet}
#' is either a \code{ListenerSet} or \code{NULL}.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{Listener}}, \code{\link{receiveMessage}},
#' \code{\link{notifyListeners}}, \code{\link[futile.logger]{flog.logger}},
#' \code{\link[mongolite]{mongo}}, \code{\linkS4class{P4Message}}
#' 
#' Listener Classes.  \code{\linkS4class{CaptureListener}},
#' \code{\linkS4class{UpdateListener}}, \code{\linkS4class{UpsertListener}},
#' \code{\linkS4class{InjectionListener}}, \code{\linkS4class{TableListener}}
#' @references \url{https://en.wikipedia.org/wiki/Observer_pattern}
#' @keywords classes
#' @examples
#' 
#' showClass("ListenerSet")
#' 
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
                    adminDB <<- mongolite::mongo("OutputFiles",admindbname,dburi)
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
                    db <<- mongolite::mongo(colname,dbname,dburi)
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


#' Builds a listener from a JSON description.
#' 
#' 
#' This is used in configuration, it will build a listener from a JSON
#' description of the listener.  The \dQuote{name} and \dQuote{type} fields are
#' required.  The other fields should match the arguments for the constructor,
#' with the exceptions noted below:
#' 
#' 
#' The input to this function is a list that comes from JSON (or some other
#' input method that returns a named list).  The \code{specs$type} field should
#' be the name of a \code{\link{Listener}} class.  This means that
#' \code{specs$type} is the name of a constructor function, and the rest of the
#' \code{spec} argument are the arguments.
#' 
#' Currently, the following fields are used.  \describe{ \item{name}{The name
#' of the listener, required.  The string \dQuote{<app>} is substituted for
#' \code{app}.} \item{type}{Required, the name of the constructor for the
#' desired class. The function will generate an error if this does not
#' correspond to the name of a class.} \item{sender}{A string insterted into
#' logged messages.  The string \dQuote{<app>} is substituted for \code{app}.}
#' \item{dburi}{If this field is non-null, then the \dQuote{dburi} field of the
#' resulting listener will be set to \code{dburi}.  Typically, this field
#' should have the value "<dburi>" if present.} \item{dbname}{The name of the
#' database in which the messages will be recorded.} \item{dbname}{The name of
#' the database collection in which the messages will be recorded.}
#' \item{messages}{A character vector giving the names of the messages the
#' listener will pay attention to.  Note that this maps to the field
#' \dQuote{messSet} in the listener object.} \item{targetField}{Used in the
#' \code{\linkS4class{UpdateListener}} and \code{\linkS4class{UpsertListener}}
#' to indicate the field to be modified.} \item{jsonEncoder}{The name of a
#' function used to encode the field value to be modified as JSON. See
#' \code{\link[EABN]{stats2json}}.} \item{qfields}{A character vector giving
#' the names of the fields used as the key for finding the message to replace.
#' Usually should contain \code{c("uid","app")}.} \item{fields}{This should be
#' a named character vector (or list) whose names indicate the names of the
#' observables/statistics to collect, and whose values are the types.  See
#' \code{\linkS4class{TableListener}}; this field maps to the
#' \dQuote{fieldlist} field of that class.} } Other fields in \code{specs} are
#' ignored.
#' 
#' @param specs A named list (from the JSON) containing the instructions for
#' building the listener.
#' @param app A character value that will get substituted for the string
#' \dQuote{<app>} in the \dQuote{name} and \dQuote{sender} fields.
#' @param dburi If a database is used for this listener, then this is the uri
#' for the connection.  Note that this is specified in the code and not in the
#' JSON.
#' @return
#' 
#' An object of the virtual class \code{\link{Listener}} (i.e., something for
#' which \code{\link{isListener}} should return true.
#' @note
#' 
#' The field name \dQuote{messages} maps to the internal field \code{messSet}.
#' The field name \dQuote{fields} maps to the internal field \code{fieldlist}.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{Listener}}, \code{\link[jsonlite]{fromJSON}}
#' @keywords interface
#' @examples
#' 
#' 
#' jspecs <- '[
#' 	{
#' 	    "name":"ppLS<app>",
#' 	    "type":"TableListener",
#' 	    "messages":["Coins Earned","Coins Spent", "LS Watched"],
#' 	    "fields":{
#' 		"uid":"character",
#' 		"context":"character",
#' 		"timestamp":"character",
#' 		"currentMoney":"numeric",
#' 		"appId":"numeric", 
#' 		"mess":"character",
#' 		"money":"numeric", 
#' 		"onWhat":"character", 
#' 		"LS_duration":"difftime",
#' 		"learningSupportType":"character"
#' 	    }
#' 	},
#' 	{
#' 	    "name":"ToEA",
#' 	    "type":"InjectionListener",
#' 	    "sender":"EI_<app>",
#' 	    "dburi":"<dburi>",
#' 	    "dbname":"EARecords",
#' 	    "colname":"EvidenceSets",
#' 	    "messages":["New Observables"]
#' 	},
#' 	{
#' 	    "name":"PPPersistantData",
#' 	    "type":"UpdateListener",
#' 	    "dburi":"<dburi>",
#' 	    "dbname":"Proc4",
#' 	    "colname":"Players",
#' 	    "targetField":"data",
#' 	    "jsonEncoder":"trophy2json",
#' 	    "messages":["Money Earned", "Money Spent"]
#' 	}
#'     ]'
#' 
#' speclist <- jsonlite::fromJSON(jspecs,FALSE)
#' 
#' l1 <- buildListener(speclist[[1]],"test","mongodb://localhost")
#' stopifnot (isListener(l1),listenerName(l1)=="ppLStest",
#'            is(l1,"TableListener"),
#'            # match("Coins Spent",l1$messSet,nomatch=0)>0,
#' TRUE)
#' 
#' l2 <- buildListener(speclist[[2]],"test","mongodb://localhost")
#' stopifnot (isListener(l2),listenerName(l2)=="ToEA",
#'            is(l2,"InjectionListener"),
#'            l2$sender=="EI_test",
#'            l2$dburi=="mongodb://localhost",
#'            #match("New Observables",l2$messSet,nomatch=0)>0,
#' TRUE)
#' 
#' 
#' l3 <- buildListener(speclist[[3]],"test","mongodb://localhost")
#' stopifnot (isListener(l3),listenerName(l3)=="PPPersistantData",
#'            is(l3,"UpdateListener"),
#'            l3$dburi=="mongodb://localhost",
#'            #match("Money Earned",l3$messSet,nomatch=0)>0,
#' TRUE)
#' 
#' 
#' 
#' @export buildListener
buildListener <- function (specs,app,dburi) {
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

#' Constructors for Listener Classes
#' 
#' 
#' These functions create objects of class
#' \code{\linkS4class{CaptureListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{UpsertListener}}, \code{\linkS4class{InjectionListener}},
#' and \code{\linkS4class{TableListener}}.
#' 
#' 
#' The functions are as follows: \describe{
#' \item{list("CaptureListener")}{Creates an object of class
#' \code{\linkS4class{CaptureListener}} which stores the messages in a list.}
#' \item{list("InjectionListener")}{Creates an object of class
#' \code{\linkS4class{InjectionListener}} which inserts the message into the
#' designated database.} \item{list("UpdateListener")}{Creates an object of
#' class \code{\linkS4class{UpdateListener}} which updates the designated
#' field.} \item{list("UpsertListener")}{Creates an object of class
#' \code{\linkS4class{UpsertListener}} which insert or replaces the message in
#' the designated collection.} \item{list("TableListener")}{Creates an object
#' of class \code{\linkS4class{TableListener}} which adds details from message
#' to rows of a data frame.} } See the class descriptions for more information.
#' 
#' @aliases CaptureListener InjectionListener UpdateListener UpsertListener
#' TableListener
#' @param messages A list into which to add the messages.
#' @param sender A character value used as the \code{\link{sender}} field of
#' the message.
#' @param dbname A character value giving the name of the database in which to
#' put the message. See \code{\link[mongolite]{mongo}}.
#' @param dburi A character vector giving the URI for the database.  See
#' \code{\link[mongolite]{mongo}}.
#' @param messSet A character vector giving the message values of the messages
#' that will be processed.  Messages whose \code{\link{mess}} value are not in
#' this list will be ignored by this listener.
#' @param colname The name of the database column into which the messages will
#' be sent.  See \code{\link[mongolite]{mongo}}.
#' @param targetField The name of the field that will be modified in the
#' database by the \code{\linkS4class{UpdateListener}}.
#' @param jsonEncoder A function that will be used to encode the data object as
#' JSON before it is set.  See \code{\linkS4class{UpdateListener}}.
#' @param qfields The fields that will be used as a key when trying to find
#' matching messages in the database for the
#' \code{\linkS4class{UpsertListener}}.
#' @param name An object of class \code{character} naming the listener.
#' @param fieldlist A named \code{character} vector giving the names and types
#' of the columns of the output matrix.  See
#' \code{\linkS4class{TableListener}}.
#' @param \dots Other arguments passed to the constructor.
#' @return
#' 
#' An object of the virtual class \code{\link{Listener}}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\linkS4class{UpsertListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{CaptureListener}},
#' \code{\linkS4class{InjectionListener}}, \code{\linkS4class{TableListener}},
#' \code{\linkS4class{ListenerSet}}, \code{\link[mongolite]{mongo}}
#' @references
#' 
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords interface database
#' @examples
#' 
#' 
#' cl <- CaptureListener()
#' 
#' il <- InjectionListener(sender="EI_app",
#'             dbname="EARecords",dburi="mongodb://localhost",
#'             colname="EvidenceSets",messSet="New Observables")
#' 
#' upsl <- UpsertListener(sender="EI_app",
#'             dbname="EARecords",dburi="mongodb://localhost",
#'             colname="LatestEvidence",messSet="New Observables",
#'             qfields=c("app","uid"))
#' 
#' trophy2json <- function(dat) {
#'   paste('{', '"trophyHall"', ':','[',
#'         paste(
#'             paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
#'                   sep=""), collapse=", "), '],',
#'         '"bankBalance"', ':', dat$bankBalance, '}')
#' }
#' ul <- UpdateListener(dbname="Proc4",dburi="mongodb://localhost",
#'             colname="Players",targetField="data",
#'             messSet=c("Money Earned","Money Spent"),
#'             jsonEncoder="trophy2json")
#' 
#' tabMaker <- TableListener(name="Trophy Table",
#'                    messSet="New Observables",
#'                    fieldlist=c(uid="character", context="character",
#'                                timestamp="character",
#'                                solvedtime="numeric",
#'                                trophy="ordered(none,silver,gold)"))
#' 
#' 
NULL
