## Listeners
## Listeners are objects that have  a receiveMessage function and can
## receive a P4Message.



#' A listener is an object which can receive a message.
#'
#'
#' A \emph{listener} an an object that takes on the observer or listener role
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
#' Typically, a listener will register itself with the speaker objects.  For
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
#' according to whether or not the object follows the listener protocol.
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
#'
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
                       db="ANY",
                       adminDB="ANY"),
              methods = list(
                  initialize =
                    function(sender="sender",
                             dbname="test",
                             admindbname="",
                             dburi="mongodb://localhost",
                             listeners=list(),
                             colname="Messages",
                             ...) {
                      callSuper(sender=sender,db=NULL,adminDB=NULL,
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

## Used in both EA and EI.
#' @describeIn ListenerSet Class union containing `"NULL"` and `"ListenerSet"`
setClassUnion("NullListenerSet",c("ListenerSet","NULL"))

