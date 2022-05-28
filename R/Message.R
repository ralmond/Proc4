###  Message -- A message send around the P4 system.

#' Class \code{"P4Message"}
#' 
#' 
#' This is a message which is sent from one process to another in the four
#' process architecture.  There are certain header fields with are used to
#' route the message and the details field which is an arbitrary list of data
#' which will can be used by the receiver.
#' 
#' This class represents a semi-structured data object with certain header
#' fields which can be indexed plus the free-form \code{details()} field which
#' contains the body of the message.  It can be serielized in JSON format
#' (using \code{\link{as.json}}) or saved in the Mongo database (using the
#' \code{\link[mongolite]{mongo}lite} package).
#' 
#' 
#' @name P4Message-class
#' @aliases P4Message-class m_id,ANY-method m_id<-,ANY-method
#' app,P4Message-method as.jlist,P4Message,list-method as.json,P4Message-method
#' context,P4Message-method context<-,P4Message-method details,P4Message-method
#' mess,P4Message-method sender,P4Message-method timestamp,P4Message-method
#' timestamp<-,P4Message-method uid,P4Message-method processed,P4Message-method
#' processingError,P4Message-method
#' @docType class
#' @section Objects from the Class:
#' 
#' Objects can be created by calls to the \code{\link{P4Message}()} function.
#' @author Russell G. Almond
#' @seealso \code{\link{P4Message}()} --- constructor
#' \code{\link{parseMessage}}, \code{\link{saveRec}}, \code{\link{getOneRec}}
#' @references
#' 
#' Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing the
#' design and delivery of Assessment Systems: A Four-Process Architecture.
#' \emph{Journal of Technology, Learning, and Assessment}, \bold{1},
#' \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}.
#' @keywords classes
#' @examples
#' 
#' showClass("P4Message")
#' 
setClass("P4Message",
         slots=c("_id"="character",    #Mongo ID
                 app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="character",    #Task or other context ID
                 sender="character",      #Which process sent the message
                 mess="character",      #Action Identifier
                 timestamp="POSIXt",      #When action took place.
                 processed="logical",     #Has this message been processed by the reciever.
                 pError="ANY",     #Error occured while processing.
                 data="list"              #More details.
                 ))

setGeneric("m_id",function(x) standardGeneric("m_id"))
setGeneric("m_id<-",function(x, value) standardGeneric("m_id<-"))
setGeneric("app",function(x) standardGeneric("app"))
setGeneric("uid",function(x) standardGeneric("uid"))
setGeneric("mess",function(x) standardGeneric("mess"))
setGeneric("context",function(x) standardGeneric("context"))
setGeneric("context<-",function(x, value) standardGeneric("context<-"))
setGeneric("sender",function(x) standardGeneric("sender"))
setGeneric("timestamp",function(x) standardGeneric("timestamp"))
setGeneric("timestamp<-",function(x, value) standardGeneric("timestamp<-"))
setGeneric("details",function(x) standardGeneric("details"))
setGeneric("processed",function(x) standardGeneric("processed"))
setGeneric("processed<-",function(x, value) standardGeneric("processed<-"))
setGeneric("processingError",function(x) standardGeneric("processingError"))
setGeneric("processingError<-",function(x, value)
  standardGeneric("processingError<-"))

setMethod("m_id","ANY", function(x) x@"_id")
setMethod("m_id<-","ANY", function(x,value) {
  x@"_id" <- value
  x})
setMethod("app","P4Message", function(x) x@app)
setMethod("uid","P4Message", function(x) x@uid)
setMethod("mess","P4Message", function(x) x@mess)
setMethod("context","P4Message", function(x) x@context)
setMethod("context<-","P4Message", function(x, value) {
  x@context <- value
  x})
setMethod("sender","P4Message", function(x) x@sender)
setMethod("timestamp","P4Message", function(x) x@timestamp)
setMethod("timestamp<-","P4Message", function(x,value) {
  x@timestamp <- as.POSIXct(value)
  x})
setMethod("details","P4Message", function(x) x@data)
setMethod("processed","P4Message", function(x) x@processed)
setMethod("processed<-","P4Message",
          function(x,value) {
            x@processed <- as.logical(value)
            x})
setMethod("processingError","P4Message", function(x) x@pError)
setMethod("processingError<-","P4Message",
          function(x,value) {
            x@pError <- value
            x})



#' Constructor and accessors for P4 Messages
#' 
#' 
#' The function \code{P4Message()} creates an object of class
#' \code{"\linkS4class{P4Message}"}.  The other functions access fields of the
#' messages.
#' 
#' 
#' This class represents a semi-structured data object with certain header
#' fields which can be indexed plus the free-form \code{details()} field which
#' contains the body of the message.  It can be serielized in JSON format
#' (using \code{\link{as.json}} in the Mongo database (using the
#' \code{\link[mongolite]{mongo}lite} package).
#' 
#' Using the public methods, the fields can be read but not set.  The generic
#' functions are exported so that other object can extend the \code{P4Message}
#' class.  The \code{m_id} function accesses the mongo ID of the object (the
#' \code{_id} field).
#' 
#' The function \code{all.equal.P4Message} checks two messages for identical
#' contents.  The flags \code{checkTimestamp} and \code{check_ids} can be used
#' to suppress the checking of those fields.  If timestamps are checked, they
#' must be within .1 seconds to be considered equal.
#' 
#' @aliases P4Message m_id m_id<- app uid mess context context<- sender
#' timestamp timestamp<- details toString,P4Message-method
#' show,P4Message-method all.equal.P4Message
#' @param uid A character object giving an identifier for the user or student.
#' @param context A character object giving an identifier for the context,
#' task, or item.
#' @param sender A character object giving an identifier for the sender.  In
#' the four-process architecture, this should be one of \dQuote{Activity
#' Selection Process}, \dQuote{Presentation Process}, \dQuote{Evidnece
#' Identification Process}, or \dQuote{Evidence Accumulation Process}.
#' @param mess A character object giving a message to be sent.
#' @param timestamp The time the message was sent.
#' @param details A list giving the data to be sent with the message.
#' @param app An identifier for the application using the message.
#' @param processed A logical flag: true if the message has been processed and
#' false otherwise.
#' @param x A message object to be queried, or converted to a string.
#' @param ... Addtional arguments for \code{\link[methods]{show}} or
#' \code{\link[base]{all.equal}}.
#' @param object A message object to be converted to a string.
#' @param target A P4Message to compare.
#' @param current A P4Message to compare.
#' @param checkTimestamp Logical flag.  If true, the timestamps are compared as
#' part of the equality test.
#' @param check_ids Logical flag.  If true, the database ids are compared as
#' part of the equality test.
#' @param value A new value for the field, type varies, but usually character.
#' @return An object of class \code{\linkS4class{P4Message}}.
#' 
#' The \code{app()}, \code{uid()}, \code{context()}, \code{sender()}, and
#' \code{mess()} functions all return a character scalar.  The
#' \code{timestamp()}, function returns an object of type \code{POSIXt} and the
#' \code{details()} function returns a list.
#' 
#' The function \code{all.equal.P4Message} returns either \code{TRUE} or a
#' vector of mode \dQuote{character} describing the differences between
#' \code{target} and \code{current}.
#' @author Russell G. Almond
#' @seealso \code{\linkS4class{P4Message}} --- class
#' \code{\link{parseMessage}}, \code{\link{saveRec}}, \code{\link{getOneRec}}
#' @references
#' 
#' Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing the
#' design and delivery of Assessment Systems: A Four-Process Architecture.
#' \emph{Journal of Technology, Learning, and Assessment}, \bold{1},
#' \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}.
#' @keywords classes
#' @examples
#' 
#' 
#' mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=TRUE,selection="D"))
#' stopifnot(
#'   app(mess1) == "default",
#'   uid(mess1) == "Fred",
#'   context(mess1) == "Task 1",
#'   sender(mess1) == "Evidence ID",
#'   mess(mess1) == "Scored Response",
#'   timestamp(mess1) == as.POSIXct("2018-11-04 21:15:25 EST"),
#'   details(mess1)$correct==TRUE,
#'   details(mess1)$selection=="D"
#' )
#' 
#' mess2 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=FALSE,selection="E"))
#' all.equal(mess1,mess2)
#' stopifnot(!isTRUE(all.equal(mess1,mess2)))
#' 
#' 
#' @export P4Message
P4Message <- function(uid,context,sender,mess,timestamp=Sys.time(),
                        details=list(),app="default", processed=FALSE) {
  new("P4Message",app=app,uid=uid,context=context,sender=sender,
      mess=mess, timestamp=timestamp,data=details,processed=processed,
      pError=NULL,
      "_id"=c(oid=NA_character_))
}

setMethod("toString","P4Message", function(x, ...) {
  paste('P4Message:{ uid:',x@uid,', context:',x@context,
        ',', x@sender, "says:", x@mess, '}')
})
setMethod("show","P4Message",function(object) {
  cat(toString(object),"\n")
})




#' Converts P4 messages to JSON representation
#' 
#' 
#' These methods extend the \code{\link[jsonlite]{toJSON}} function providing
#' an extensible protocol for serializing S4 objects.  The function
#' \code{as.json} turns the object into a string containing a JSON document by
#' first calling \code{as.jlist} to convert the object into a list and then
#' calling \code{toJSON} to do the work.
#' 
#' 
#' The existing \code{\link[jsonlite]{toJSON}} does not support S4 objects, and
#' the \code{\link[jsonlite]{serializeJSON}} provides too much detail; so while
#' it is good for saving and restoring R objects, it is not good for sharing
#' data between programs.  The function \code{as.json} and \code{as.jlist} are
#' S4 generics, so they can be easily extended to other classes.
#' 
#' The default method for \code{as.json} is essentially \code{toJSON(
#' as.jlist(x, attributes(x)))}.  The function \code{attributes(x)} turns the
#' fields of the object into a list, and then the appropriate method for
#' \code{as.jlist} further processes those objects.  For example, it can set
#' the \code{"_id"} field used by the Mongo DB as a unique identifier (or other
#' derived fields) to \code{NULL}.
#' 
#' Another important step is to call \code{unboxer} on fields which should not
#' be stored as vectors.  The function \code{toJSON} by default wraps all R
#' objects in \sQuote{[]} (after all, they are all vectors), but that is
#' probably not useful if the field is to be used as an index.  Wrapping the
#' field in \code{unboxer()}, i.e., using \code{ml$field <- unboxer(ml$field)},
#' suppresses the brackets.  The function \code{unboxer()} in this package is
#' an extension of the \code{jsonlite::\link[jsonlite]{unbox}} function, which
#' does not properly unbox POSIXt objects.
#' 
#' Finally, for a field that can contain arbitrary R objects, the function
#' \code{\link{unparseData}} coverts the data into a JSON string which will
#' completely recover the data.  The \code{serialize} argument is passed to
#' this function.  If true, then \code{\link[jsonlite]{serializeJSON}} is used
#' which produces safe, but not particularly human editable JSON.  If false, a
#' simpler method is employed which produes more human readable code.  This
#' with should work for simpler data types, but does not support objects, and
#' may fail with complex lists.
#' 
#' @aliases as.json as.json,ANY-method as.jlist
#' @param x An (S4) object to be serialized.
#' @param obj The object being serialized
#' @param ml A list of fields of the object; usually \code{attributes(obj)}.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#' 
#' The function \code{as.json} returns a unicode string with a serialized
#' version of the object.
#' 
#' The function \code{as.jlist} returns a list of the fields of the object
#' which need to be serialized (usually through a call to
#' \code{\link[jsonlite]{toJSON}}.
#' @author Russell Almond
#' @seealso In this package: \code{\link{parseMessage}}, \code{\link{saveRec}},
#' \code{\link{parseData}}
#' 
#' In the jsonlite package: \code{\link[jsonlite]{toJSON}},
#' \code{\link[jsonlite]{serializeJSON}},
#' \code{jsonlite::\link[jsonlite]{unbox}}
#' @keywords IO interfaces
#' @examples
#' 
#' mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=TRUE,seletion="D"))
#' as.json(mess1)
#' as.json(mess1,FALSE)
#' 
#' \dontrun{
#' ## This is the method for P4 Messages.
#' setMethod("as.jlist",c("P4Message","list"), function(obj,ml) {
#'   ml$"_id" <- NULL
#'   ml$class <-NULL
#'   ## Use manual unboxing for finer control.
#'   ml$app <- unboxer(ml$app)
#'   ml$uid <- unboxer(ml$uid)
#'   if (!is.null(ml$context) && length(ml$context)==1L)
#'     ml$context <- unboxer(ml$context)
#'   if (!is.null(ml$sender) && length(ml$sender)==1L)
#'     ml$sender <- unboxer(ml$sender)
#'   if (!is.null(ml$mess) && length(ml$mess)==1L)
#'     ml$mess <- unboxer(ml$mess)
#'   ml$timestamp <- unboxer(ml$timestamp) # Auto_unboxer bug.
#'   ## Saves name data; need recursvie version.
#'   ml$data <- unparseData(ml$data)
#'   ml
#'   })
#' }
#' 
#' 
setGeneric("as.json",function(x,serialize=TRUE) standardGeneric("as.json"))
setGeneric("as.jlist",function(obj,ml,serialize=TRUE)
  standardGeneric("as.jlist"))

setMethod("as.json","ANY", function(x,serialize=TRUE) {
  jlist <- as.jlist(x,attributes(x),serialize)
  toJSON(jlist,POSIXt="mongo")
})

setMethod("as.jlist",c("P4Message","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <-NULL
  ## Use manual unboxing for finer control.
  ml$app <- unboxer(ml$app)
  ml$uid <- unboxer(ml$uid)
  ml$processed <- unboxer(ml$processed)
  if (!is.null(ml$context) && length(ml$context)==1L)
    ml$context <- unboxer(ml$context)
  if (!is.null(ml$sender) && length(ml$sender)==1L)
    ml$sender <- unboxer(ml$sender)
  if (!is.null(ml$mess) && length(ml$mess)==1L)
    ml$mess <- unboxer(ml$mess)
  ml$timestamp <- unboxer(ml$timestamp) # Auto_unbox bug.
  ## Saves name data
  ml$data <- unparseData(ml$data,serialize)
  ## explicit null value creates problem, so drop pError if null.
  ## note attributes maps NULL to a symbol '\001NULL\001'
  if (is.null(ml$pError) || ml$pError == '\001NULL\001') {
    ml["pError"] <- NULL
  } else {
    ml$pError <- unboxer(toString(ml$pError))
  }
  ml
  })



#' Saves a P4 Message object to a Mongo database
#' 
#' 
#' This function saves an S4 object as a record in a Mongo databalse.  It uses
#' \code{\link{as.json}} to covert the object to a JSON string.
#' 
#' 
#' @param mess The message (object) to be saved.
#' @param col A mongo collection object, produced with a call to
#' \code{\link[mongolite]{mongo}()}. This can also be \code{NULL}, in which
#' case the message will not be saved.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#' 
#' Returns the message argument, which may be modified by setting the
#' \code{"_id"} field if this is the first time saving the object.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{as.json}}, \code{\linkS4class{P4Message}},
#' \code{\link{parseMessage}}, \code{\link{getOneRec}},
#' \code{\link[mongolite]{mongo}}
#' @keywords database
#' @examples
#' 
#' 
#' \dontrun{## Need to set up database or code won't run.
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' 
#' ## Save them back to capture the ID.
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' 
#' 
#' 
#' }
#' 
#' @export saveRec
saveRec <- function (mess, col, serialize=TRUE) {
  if (!is.null(col)) {
    jso <- as.json(mess,serialize)
    if (is.na(mess@"_id")) {
      ## Insert
      col$insert(jso)
      it <- col$iterate(jso,'{"_id":true}',limit=1)
      mess@"_id" <- it$one()$"_id"
      names(mess@"_id") <- "oid" ## Aids in extraction
    } else {
      if (col$count(paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""))) {
        ## Replace
        col$update(paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
                   paste('{"$set":',jso,'}',sep=""))
      } else {
        ## ID is out of date, insert and get new ID.
        col$insert(jso)
        it <- col$iterate(jso,'{"_id":true}',limit=1)
        mess@"_id" <- it$one()$"_id"
        names(mess@"_id") <- "oid" ## Aids in extraction
      }
    }
  } else {
    flog.trace("DB is null, not saving message.")
  }
  mess
}



#' Functions for manipulating entries in a message queue.
#' 
#' 
#' A collection of message objects can serve as a queue: they can be sorted by
#' their \code{\link{timestamp}} and then processed one at a time.  The
#' function \code{markAsProcessed} sets the processed flag on the message and
#' then saves it back to the database.  The function \code{processed} returns
#' the processed flag.
#' 
#' The function \code{markAsError} attaches an error to the message and saves
#' it.  The function \code{processingError} returns the error (if it exists).
#' 
#' 
#' A \code{\link[mongolite]{mongo}} collection of messages can serve as a
#' queue.  As messages are added into the queue, the \code{processed} flag is
#' set to false.  The handler then fetches them one at a time (sorting by the
#' timestamp).  It then does whatever action is required to handle the message.
#' Then the function \code{markAsProcessed} is called to set the
#' \code{processed} flag to true and update the entry in the database.
#' 
#' A typical query (this example is taken from the
#' \code{\link[EIEvent]{EIEvent-package}}) is
#' \code{getOneRec(buildJQuery(app=app, processed=FALSE), eventdb(),
#' parseEvent, sort = c(timestamp = 1))}.  Here the \code{\link{buildJQuery}}
#' call searches for unprocessed events corresponding to a particular
#' \code{\link{app}}.  The \code{sort} argument ensures that the records will
#' be sorted in ascending order according to \code{\link{timestamp}}.  In this
#' example \code{eventdb()} in an internal method which returns the event
#' collection, and \code{\link[EIEvent]{parseEvent}} create event objects
#' (which are a subclass of \code{\linkS4class{P4Message}}.
#' 
#' Some thought needs to be given as to how to handle errors.  The function
#' \code{markAsError} attaches an error object to the message and then updates
#' it in the collection.  The error object is turned into a string (using
#' \code{\link[base]{toString}}) before saving, so it can be any type of R
#' object (in particular, it could be either the error message or the actual
#' error object thrown by the function).
#' 
#' @aliases processed processingError markAsProcessed markAsError
#' @param mess An object of class \code{\linkS4class{P4Message}} to be
#' modified.
#' @param col A \code{\link[mongolite]{mongo}} collection where the message
#' queue is stored.  This can also be \code{NULL} in which case the message
#' will not be saved to the database.
#' @param e An object indicating the error occurred.  Note this could be either
#' a string giving the error message of an object of an error class.  In either
#' case, it is converted to a string before saving.
#' @param x A message object to be queried.
#' @return
#' 
#' The functions \code{markAsProcessed} and \code{markAsError} both return the
#' modified message.
#' 
#' The function \code{processed} returns a logical value indicating whether or
#' not the message has been processed.
#' 
#' The function \code{processingError} returns the error object attached to the
#' message, or \code{NULL} if no error object is returned.  Note that the error
#' object could be of any type.
#' @note
#' 
#' The functions \code{markAsProcessed} and \code{markAsError} do not save the
#' complete record, they just update the processed or error field.
#' 
#' There was a bug in early version of this function, which caused the error to
#' be put into a list when it was saved.  This needs to be carefully checked.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\linkS4class{P4Message}}, \code{\link{getOneRec}},
#' \code{\link{buildJQuery}}, \code{\link{timestamp}}
#' @keywords interface database
#' @examples
#' 
#' 
#' \dontrun{
#' col <- mongolite::mongo("TestMessages")
#' col$remove('{}')             # Clear out anything else in queue.
#' mess1 <- P4Message("One","Adder","Tester","Add me",app="adder",
#'                    details=list(x=1,y=1))
#' mess2 <- P4Message("Two","Adder","Tester","Add me",app="adder",
#'                    details=list(x="two",y=2))
#' mess1 <- saveRec(mess1,col,FALSE)
#' mess2 <- saveRec(mess2,col,FALSE)
#' 
#' mess <- getOneRec(buildJQuery(app="adder", processed=FALSE),
#'     col, parseMessage, sort = c(timestamp = 1))
#' iterations <- 0
#' while (!is.null(mess)) {
#'   if (iterations > 4L)
#'     stop ("Test not terminating, flag not being set?")
#'   iterations <- iterations + 1
#'   print(mess)
#'   print(details(mess))
#'   out <- try(print(details(mess)$x+details(mess)$y))
#'   if (is(out,'try-error'))
#'    mess <- markAsError(mess,col,out)
#'   mess <- markAsProcessed(mess,col)
#'   mess <- getOneRec(buildJQuery(app="adder", processed=FALSE),
#'     col, parseMessage, sort = c(timestamp = 1))
#' 
#' }
#' 
#' mess1a <- getOneRec(buildJQuery(app="adder",uid="One"),col,parseMessage)
#' mess2a <- getOneRec(buildJQuery(app="adder",uid="Two"),col,parseMessage)
#' stopifnot(processed(mess1a),processed(mess2a),
#'           is.null(processingError(mess1a)),
#'           grepl("Error",processingError(mess2a)))
#' }
#' 
#' @export markAsProcessed
markAsProcessed <- function (mess,col) {
  processed(mess) <- TRUE
  if (!is.null(col)) {
    col$update(paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
               '{"$set": {"processed":true}}')
  } else {
    flog.trace("DB is null, not saving message.")
  }
  mess
}

markAsError <- function (mess,col, e) {
  processingError(mess) <- e
  if (!is.null(col)) {
    col$update(paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
               paste('{"$set": {"pError":"',
                     chartr("\"","'",     #Problem with interior quotes.
                            encodeString(toString(e))),'"}}',sep=""))
  } else {
    flog.trace("DB is null, not saving message.")
  }
  mess
}



## as.vector suppresses the names which are harmless, but make writing
## test suites harder.

## The cleanning code gets reused by other classes which inherit from
## P4Message.
## toJSON | fromJSON on an empty list will change the type, so need to
## check for empty lists.
cleanMessageJlist <- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  if (is.list(rec$"_id")) rec$"_id" <- rec$"_id"$`$oid`
  names(rec$"_id") <- "oid"
  if (is.null(rec$app) || length(rec$app) == 0L) rec$app <- "default"
  if (is.null(rec$context) || length(rec$context) == 0L) rec$context <-""
  rec$context <- trimws(as.character(rec$context))
  if (is.null(rec$mess) || length(rec$mess) == 0L) rec$mess <-""
  rec$mess <- trimws(as.character(rec$mess))
  if (is.null(rec$sender)|| length(rec$sender) == 0L) rec$sender <-""
  if (is.null(rec$processed)) rec$processed <- FALSE
  if (is.null(rec$timestamp)) rec$timestamp <- Sys.time()
  if (is.list(rec$timestamp)) rec$timestamp <- rec$timestamp$`$date`
  if (!is.null(rec$pError) && !is.character(rec$pError)) {
    ## Fix old data which did not have unbox around it.
    rec$pError <- as.character(rec$pError)
  }
  rec
}



#' Converts a JSON object into a P4 Message
#' 
#' 
#' The \code{parseMessage} function is a parser to use with the
#' \code{\link{getOneRec}} and \code{\link{getManyRecs}} database query
#' functions.  This function will convert the documents fetched from the
#' database into \code{\linkS4class{P4Message}} objects.  The function
#' \code{parseData} is a helper function for parsing the \code{data} field of
#' the \code{P4Message} object, and \code{unparseData} is its inverse.
#' 
#' 
#' The \code{$iterator()} method of the \code{\link[mongolite]{mongo}} object
#' returns a list containing the fields of the JSON object with a
#' \emph{name}=\emph{value} format.  This is the \code{rec} argument.  The
#' \code{parseMessage} function takes the fields of the JSON object and uses
#' them to populate a corresponding \code{\linkS4class{P4Message}} object.
#' Usually, some cleaning is done first (e.g., to check the argument types and
#' insert default values).  The function \code{cleanMessageJlist} does that
#' cleaning for the common fields of the \code{P4Message} object, so subclasses
#' \code{P4Message} can inheret the parsing for the commond message fields.
#' 
#' The \code{data} field needs extra care as it could contain arbitrary R
#' objects.  There are two strategies for handling the data field.  First, use
#' \code{\link[jsonlite]{serializeJSON}} to turn the data field into a slob
#' (string large object), and \code{\link[jsonlite]{unserializeJSON}} to decode
#' it.  This strategy should cover most special cases, but does not result in
#' easily edited JSON output.  Second, recursively apply \code{\link{unboxer}}
#' and use the function \code{parseSimpleMessage} to undo the coding.  This
#' results in output which should be more human readable, but does not handle
#' objects (either S3 or S4).  It also may fail on more complex list
#' structures.
#' 
#' @aliases parseMessage parseData parseSimpleData unparseData
#' cleanMessageJlist
#' @param rec A named list containing JSON data.
#' @param messData A named list containing JSON data.
#' @param data An R object to be serialized.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#' 
#' The function \code{parseMessage} returns a \code{\linkS4class{P4Message}}
#' object populated with fields from the \code{rec} argument.  The function
#' \code{cleanMessageJlist} returns the cleaned \code{rec} argument.
#' 
#' The function \code{unparseData} returns a JSON string representing the data.
#' The functions \code{parseData} and \code{parseSimpleData} return a list
#' containing the data.
#' @note
#' 
#' I hit the barrier pretty quickly with trying to unparse the data manually.
#' In particular, it was impossible to tell the difference between a list of
#' integers and a vector of integers (or any other storage type).  So, I went
#' with the serialize solution.
#' 
#' The downside of the serial solution is that it stores the data field as a
#' slob.  This means that data values cannot be indexed.  If this becomes a
#' problem, a more complex implementation may be needed.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{as.jlist}}, \code{\link{getOneRec}}, \code{\link{getManyRecs}},
#' \code{\linkS4class{P4Message}}
#' 
#' \code{\link[mongolite]{mongo}}, \code{\link[jsonlite]{serializeJSON}},
#' \code{\link[jsonlite]{unserializeJSON}}
#' @keywords interface database
#' @examples
#' 
#' 
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' ev1 <- P4Message("Phred","Level 1","PP","Task Done",
#'       timestamp=as.POSIXct("2018-12-21 00:01:01"),
#'       details=list("list"=list("one"=1,"two"=1:2),"vector"=(1:3)))
#' 
#' 
#' m1a <- parseMessage(ununboxer(as.jlist(m1,attributes(m1))))
#' m2a <- parseMessage(ununboxer(as.jlist(m2,attributes(m2))))
#' m3a <- parseMessage(ununboxer(as.jlist(m3,attributes(m3))))
#' 
#' ev1a <- parseMessage(ununboxer(as.jlist(ev1,attributes(ev1))))
#' 
#' stopifnot(all.equal(m1,m1a),
#'           all.equal(m2,m2a),
#'           all.equal(m3,m3a),
#'           all.equal(ev1,ev1a))
#' 
#' \dontrun{ #Requires test DB setup.
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' testcol$remove('{}')  ## Clear everything for test.                  
#' 
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' ev1 <- saveRec(ev1,testcol)
#' 
#' m1 <- saveRec(m1,testcol)
#' m1b <- getOneRec(buildJQuery("_id"=m_id(m1)),testcol,parseMessage)
#' stopifnot(all.equal(m1,m1b))
#' m23 <- getManyRecs(buildJQuery("uid"="Fred",sender=c("EI","EA")),
#'                   testcol,parseMessage)
#' stopifnot(length(m23)==2L)
#' ev1b <- getOneRec(buildJQuery("uid"="Phred"),
#'                   testcol,parseMessage)
#' stopifnot(all.equal(ev1,ev1b))
#' 
#' }
#' 
#' @export parseMessage
parseMessage<- function (rec) {
  rec <- cleanMessageJlist(rec)
  ## Need to force the `oid` label on to match specs.
  id <- as.character(ununboxer(rec$"_id"))
  if (is.null(names(id)))  names(id) <- "oid"
  new("P4Message","_id"=id,
      app=as.character(ununboxer(rec$app)),
      uid=as.character(ununboxer(rec$uid)),
      context=as.vector(ununboxer(rec$context)),
      sender=as.character(ununboxer(rec$sender)),
      mess=as.character(ununboxer(rec$mess)),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      processed=as.logical(ununboxer(rec$processed)),
      pError=rec$pError,
      data=parseData(rec$data))
}


## Older and simpler parser, but this might work with non-serialized
## content.
parseSimpleData <- function (messData) {
  ##Need to convert back from list to numeric/character
  if (length(messData) == 0L) return(list())
  for (i in 1:length(messData)) {
    datum <- messData[[i]]
    if (all(sapply(datum,is.character)) && all(sapply(datum,length)==1L)) {
      datum <- as.character(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.logical)) && all(sapply(datum,length)==1L)) {
      datum <- as.logical(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.numeric)) && all(sapply(datum,length)==1L)) {
      if (all(sapply(datum,is.integer))) {
        datum <- as.integer(datum)
      } else {
        datum <- as.numeric(datum)
      }
      names(datum) <- names(messData[[i]])
    }
    ## May need an extra step here to decode data which
    ## are not one of the primative vector types.
    messData[[i]] <- datum
  }
  messData
}

parseData <- function (messData) {
  if (is.character(messData)) {
    unserializeJSON(messData)
  } else {
    parseSimpleData(messData)
  }
}

unparseData <- function (data,serialize=TRUE) {
  if (serialize)
    unbox(serializeJSON(data))
  else
    unboxer(data)
}

mongoQueries <- c("eq","gt","gte","lt","lte","ne","nin","in","","oid")

#### Need to override jsonlite::unbox as it doesn't properly handle POSIXt objects.


#' Marks scalar objects to be preserved when converting to JSON
#' 
#' 
#' The function \code{\link[jsonlite]{toJSON}} coverts vectors (which all R
#' objects are) to vectors in the JSON code.  The function
#' \code{jsonlite::\link[jsonlite]{unbox}} protects the object from this
#' behavior, which makes the fields eaiser to search and protects against loss
#' of name attributes.  The function \code{unboxer} extents \code{unbox} to
#' recursively unbox lists (which preserves names).  The function
#' \code{ununbox} removes the unboxing flag and is mainly used for testing
#' parser code.
#' 
#' 
#' The \code{jsonlite::\link[jsonlite]{unbox}} function does not necessarily
#' preserve the name attributes of elements of the list.  In other words the
#' sequence \code{\link{as.jlist}} -> \code{\link[jsonlite]{toJSON}} ->
#' \code{\link[jsonlite]{fromJSON}} -> \code{\link{parseMessage}} might not be
#' the identity.
#' 
#' The solution is to recursively apply \code{\link[jsonlite]{unbox}} to the
#' elements of the list.  The function \code{unboxer} can be thought of as a
#' recursive version of \code{unbox} which handles the entire tree struction.
#' If \code{x} is not a list, then \code{unboxer} and \code{unbox} are
#' equivalent.
#' 
#' The typical use of this function is defining methods for the
#' \code{\link{as.jlist}} function.  This gives the implementer fine control of
#' which attributes of a class should be scalars and vectors.
#' 
#' The function \code{ununbox} clears the unboxing flag.  Its main purpose is
#' to be able to test various parsers.
#' 
#' @aliases unboxer ununboxer
#' @param x Object to be boxed/unboxed.
#' @return The function \code{unboxer} returns the object with the added class
#' \code{scalar}, which is the \code{jsonlite} marker for a scalar.
#' 
#' The function \code{ununboxer} returns the object without the \code{scalar}
#' class marker.
#' @note
#' 
#' There is a bug in the way that \code{\link[base]{POSIXt}} classes are
#' handled, \code{unboxer} fixes that problem.
#' @section Warning: Dependence on jsonlite implementation:
#' 
#' These functions currently rely on some internal mechanisms of the jsonline
#' pacakge.  In particular, it uses the internal function
#' \code{jsonlite:::as.scalar}, and \code{ununbox} relies on the
#' \dQuote{scalar} class mechanism.
#' @author Russell Almond
#' @seealso \code{\link[jsonlite]{unbox}}, \code{\link[jsonlite]{toJSON}},
#' \code{\link{as.jlist}}, \code{\link{parseMessage}}
#' @keywords interface
#' @examples
#' 
#' 
#' ## as.jlist method shows typical use of unboxer.
#' getMethod("as.jlist",c("P4Message","list"))
#' 
#' ## Use ununboxer to test as.jlist/parseMessage pair.
#' m4 <- P4Message("Phred","Task1","PP","New Stats",
#'                 details=list("agents"=c("ramp","ramp","lever")))
#' m4jl <- as.jlist(m4,attributes(m4))
#' m4a <- parseMessage(ununboxer(m4jl))
#' stopifnot(all.equal(m4,m4a))
#' 
#' 
#' 
#' @export unboxer
unboxer <- function (x) {
  if (length(x) == 1L && is(x,"POSIXt")) {
    jsonlite:::as.scalar(x)
  } else if (is(x,"list")) {
    lapply(x,function (s) lapply(s,unboxer)) #Saves name data.
  } else {
    if (length(x) == 1L) {
      jsonlite::unbox(x)
    } else {
      x
    }
  }
}

## Need this for testing.
ununboxer <- function (x) {
  if (is(x,"scalar"))
    class(x) <- setdiff(class(x),"scalar")
  if (is.list(x))
    x <- lapply(x, function(s) {
      if (is(s,"POSIXt")) {
        ununboxer(s)
      } else {
        sapply(s,ununboxer)
      }})
  x
}

buildJQterm <- function (name,value) {
  if (length(value)==0L)
    stop("Query term ",name,"has no value.")
  compOps <- names(value)
  names(value) <- NULL
  if (is.null(compOps)) {
    if (length(value) == 1L) {
      ## Singleton query.
      vstring <- toJSON(unboxer(value),POSIXt="mongo")
    } else {
      ## Unmarked $in query
      vstring <- paste('{"$in":',toJSON(unlist(value),
                                        POSIXt="mongo"),'}',sep="")
    }
  } else {
    compOps <- sub('^\\$','',compOps)   #Strip leading $
    if(!all(compOps %in% mongoQueries)) {
      stop("Unspported operator ",compOps[!(compOps%in%mongoQueries)],
           " in query for field ",name)
    }
    if(compOps[1]=="nin" || compOps[1]=="in" || compOps[1]=="") {
      ## Special Handling for (n)in query)
      op <- ifelse(compOps[1]=="","in",compOps[1])
      vstring <- paste('{"$',op,'":',toJSON(unlist(value),
                                            POSIXt="mongo"),'}',sep="")
    } else {
      ## iterate over values.
      vstring <- sapply(1:length(compOps),
                        function (i)
                          paste('"$',compOps[i],'":',
                                toJSON(unboxer(value[i]),POSIXt="mongo"),
                                sep=""))
      vstring <- paste('{',paste(vstring,collapse=", "),'}')
    }
  }
  paste('"',name,'":',vstring,sep="")
}



#' Transforms a query into JQuery JSON.
#' 
#' 
#' This function takes a query which is expressed in the argument list and
#' transforms it into a JSON query document which can be used with the Mongo
#' Database.  The function \code{buildJQterm} is a helper function which builds
#' up a single term of the query.
#' 
#' 
#' A typical query to a Mongo database collection is done with a JSON object
#' which has a number of bits that look like
#' \dQuote{\emph{field}:\emph{value}}, where \emph{field} names a field in the
#' document, and \emph{value} is a value to be matched.  A record matches the
#' query if all of the fields specified in the query match the corresponding
#' fields in the record.
#' 
#' Note that \emph{value} could be a special expression which gives specifies a
#' more complex expression allowing for ranges of values.  In particular, the
#' Mongo query language supports the following operators: \code{"$eq", "$ne",
#' "$gt", "$lt", "$gte", "$lte"}.  These can be specified using a value of the
#' form \code{c(<op>=<value>)}, where \emph{op} is one of the mongo operators,
#' without the leading \sQuote{$}.  Multiple op--value pairs can be specified;
#' for example, \code{count=c(gt=3,lt=6)}.  If no op is specified, then
#' \code{"$eq"} is assumed.  Additionally, the \code{"$oid"} operator can be
#' used to specify that a value should be treated as a Mongo record identifier.
#' 
#' The \code{"$in"} and \code{"$nin"} are also ops, but the corrsponding value
#' is a vector.  They test if the record is in or not in the specified value.
#' If the value is vector valued, and no operator is specified it defaults to
#' \code{"$in"}.
#' 
#' The function \code{buildJQuery} processes each of its arguments, adding them
#' onto the query document.  The \code{rawfields} argument adds the fields onto
#' the document without further processing.  It is useful for control arugments
#' like \code{"$limit"} and \code{"$sort"}.
#' 
#' @aliases buildJQuery buildJQterm
#' @param \dots This should be a named list of arguments.  The values should be
#' the desired query value, or a more complex expression (see details).
#' @param rawfields These arguments are passed as character vectors directly
#' into the query document without processing.
#' @param name The name of the field.
#' @param value The value of the field or an expression which gives a query for
#' the resulting document.
#' @return The function \code{buildJQuery} returns a unicode string which
#' contains the JSON query document.  The function \code{buildJQterm} returns a
#' unicode string with just one field in the query document.
#' @author Russell Almond
#' @seealso \code{\link{as.json}}, \code{\link{parseMessage}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#' 
#' 
#' ## Low level test of the JQterm possibilities for fields.
#' 
#' stopifnot(buildJQterm("uid","Fred")=='"uid":"Fred"')
#' stopifnot(buildJQterm("uid",c("Phred","Fred"))=='"uid":{"$in":["Phred","Fred"]}')
#' time1 <- as.POSIXct("2018-08-16 19:12:19 EDT")
#' stopifnot(buildJQterm("time",time1)=='"time":{"$date":1534461139000}')
#' time1l <- as.POSIXlt("2018-08-16 19:12:19 EDT")
#' stopifnot(buildJQterm("time",time1l)=='"time":{"$date":1534461139000}')
#' time2 <- as.POSIXct("2018-08-16 19:13:19 EDT")
#' stopifnot(buildJQterm("time",c(time1,time2))==
#'           '"time":{"$in":[{"$date":1534461139000},{"$date":1534461199000}]}')
#' stopifnot(buildJQterm("time",c(gt=time1))==
#'           '"time":{ "$gt":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(lt=time1))==
#'           '"time":{ "$lt":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(gte=time1))==
#'           '"time":{ "$gte":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(lte=time1))==
#'           '"time":{ "$lte":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(ne=time1))==
#'           '"time":{ "$ne":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(eq=time1))==
#'           '"time":{ "$eq":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(gt=time1,lt=time2))==
#'           '"time":{ "$gt":{"$date":1534461139000}, "$lt":{"$date":1534461199000} }')
#' stopifnot(buildJQterm("count",c(nin=1,2:4))==
#'           '"count":{"$nin":[1,2,3,4]}')
#' stopifnot(buildJQterm("count",c("in"=1,2:4))==
#'           '"count":{"$in":[1,2,3,4]}')
#' stopifnot(buildJQterm("count",c(ne=1,ne=5))==
#'           '"count":{ "$ne":1, "$ne":5 }')
#' 
#' ## Some Examples of buildJQuery on complete queries.
#' 
#' stopifnot(buildJQuery(app="default",uid="Phred")==
#'           '{ "app":"default", "uid":"Phred" }')
#' stopifnot(buildJQuery("_id"=c(oid="123456789"))==
#'           '{ "_id":{ "$oid":"123456789" } }')
#' stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5))==
#'           '{ "name":"George", "count":{ "$gt":3, "$lt":5 } }')
#' stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5),
#'                       rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))==
#'           '{ "name":"George", "count":{ "$gt":3, "$lt":5 }, "$limit":1, "$sort":{timestamp:-1} }')
#' 
#' 
#' ## Queries on IDs need special handling
#' stopifnot(buildJQuery("_id"=c(oid="123456789abcdef"))==
#'           '{ "_id":{ "$oid":"123456789abcdef" } }')
#' 
#' 
#' 
#' 
#' @export buildJQuery
buildJQuery <- function (...,rawfields=character()) {
  terms <- list(...)
  fields <- names(terms)
  jstrings <- sapply(fields,function(f) buildJQterm(f,terms[[f]]))
  jstrings <- c(jstrings,rawfields)
  query <- paste('{',paste(jstrings,collapse=", "),'}')
  flog.trace("Query = ",query,capture=TRUE)
  query
}




#' Fetches Messages from a Mongo databas
#' 
#' 
#' This function fetches \code{\linkS4class{P4Message}} objects from a
#' \code{\link[mongolite]{mongo}} database.  The message parser is passed as an
#' argument, allowing it to fetch other kinds of objects than P4Messages.  The
#' function \code{getManyRecs} retrieves all matching objects and the function
#' \code{getOneRec} retrieves the first matching object.
#' 
#' 
#' This function assumes that a number of objects (usually, but not necessarily
#' subclasses of \code{\link{P4Message}} objects) have been stored in a Mongo
#' database.  The \code{col} argument is the \code{\link[mongolite]{mongo}}
#' object in which they are stored.  These functions retrive the selected
#' objects.
#' 
#' The first argument should be a string containing a JSON query document.
#' Normally, thes are constructed through a call to \code{\link{buildJQuery}}.
#' 
#' The query is used to create an iterator over JSON documents stored in the
#' database.  At each round, the iterator extracts the JSON document as a
#' (nested) list structure.  This is pased to the \code{parser} function to
#' build an object of the specified type.  See the \code{\link{parseMessage}}
#' function for an example parser.
#' 
#' The sorting argument controls the way the returned list of objects is
#' sorted. This should be a numeric vector with names giving the field for
#' sorting.  The default values \code{c("timestamp"=1)} and
#' \code{c("timestamp"=-1)} sort the records in ascending and decending order
#' respectively.  In particular, the default value for \code{getOneRec} means
#' that the most recent value will be returned.  The defaults assume that
#' \dQuote{timestamp} is a field of the stored object.  To supress sorting of
#' outputs, use \code{NULL} as the argument to \code{sort}.
#' 
#' @aliases getOneRec getManyRecs
#' @param jquery A string providing a Mongo JQuery to select the appropriate
#' records.  See \code{\link{buildJQuery}}.
#' @param col A \code{\link[mongolite]{mongo}} collection object to be queried.
#' @param parser A function which will take the list of fields returned from
#' the database and build an appropriate R object.  See
#' \code{\link{parseMessage}}.
#' @param sort A named numeric vector giving sorting instructions.  The names
#' should correpond to fields of the objects, and the values should be positive
#' or negative one for increasing or decreasing order. Use the value
#' \code{NULL} to leave the results unsorted.
#' @param limit A numeric scalar giving the maximum number of objects to
#' retrieve.  If 0, then all objects matching the query will be retrieved.
#' @return
#' 
#' The function \code{getOneRec} returns an object whose type is determined by
#' the output of the \code{parser} function.  If \code{\link{parseMessage}} is
#' used, this will be a \code{\linkS4class{P4Message}} object.
#' 
#' The function \code{getManyRecs} returns a list of object whose type is
#' determined by the output of the \code{parser} function.
#' @author Russell Almond
#' @seealso \code{\link{saveRec}}, \code{\link{parseMessage}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#' 
#' 
#' \dontrun{
#' ## Requires Mongo test database to be set up.
#' 
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' ## collection = Messages
#' ## Execute in Mongo Shell
#' ## db.createUser({
#' ## ... user: "test",
#' ## ... pwd: "secret",
#' ## ... roles: [{role: "readWrite", db: "test"}]
#' ## ... });
#' 
#' 
#' 
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' 
#' m1@data$time <- list(tim=25.4,units="secs")
#' m1 <- saveRec(m1,testcol)
#' 
#' ## Note use of oid keyword to fetch object by Mongo ID.
#' m1a <- getOneRec(buildJQuery("_id"=c(oid=m1@"_id")),testcol,parseMessage)
#' stopifnot(all.equal(m1,m1a))
#' 
#' m123 <- getManyRecs(buildJQuery(uid="Fred"),testcol,parseMessage)
#' m23 <- getManyRecs(buildJQuery(uid="Fred",sender=c("EI","EA")),
#'                    testcol,parseMessage)
#' m321 <- getManyRecs(buildJQuery(uid="Fred",timestamp=c(lte=Sys.time())),
#'             testcol,parseMessage,sort=c(timestamp=-1))
#' getManyRecs(buildJQuery(uid="Fred",
#'                         timestamp=c(gte=Sys.time()-as.difftime(1,units="hours"))),
#'                         testcol,parseMessage)
#' }
#' 
#' 
#' @export getOneRec
getOneRec <- function(jquery,col,parser,sort=c("timestamp"=-1)) {
  sorts <- paste('{',paste(paste('"',names(sort),'":',sort,sep=""),
                           collapse=", "),'}')
  it <- col$iterate(jquery,'{}',sort=sorts,limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  do.call(parser,list(rec))
}

getManyRecs <- function(jquery,col,parser,sort=c("timestamp"=1),
                        limit = 0) {
  sorts <- paste('{',paste(paste('"',names(sort),'":',sort,sep=""),
                           collapse=", "),'}')

  n <- col$count(jquery)
  if (limit>0) n <- min(n,limit)
  result <- vector("list",n)
  it <- col$iterate(jquery,'{}',sort=sorts,limit=limit)
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- do.call(parser,list(rec))
    nn <- nn +1
  }
  result
}

all.equal.P4Message <- function (target, current, ...,checkTimestamp=FALSE,check_ids=TRUE) {
  if (!is(current,"P4Message"))
    return(paste("Target is 'P4Message' and current is '",class(current),"'."))
  msg <- character()
  if (check_ids)
    if ((is.na(target@"_id") && !is.na(current@"_id")) ||
        (!is.na(target@"_id") &&
         !isTRUE(all.equal(target@"_id", current@"_id"))))
      msg <- c(msg,"Database IDs do not match.")
  if (app(target) != app(current))
    msg <- c(msg,"Application IDs do not match.")
  if (uid(target) != uid(current))
    msg <- c(msg,"User IDs do not match.")
  if (context(target) != context(current))
    msg <- c(msg,"Contexts do not match.")
  if (!(length(sender(target))==0L && length(sender(current))==0L) &&
      any(sender(target) != sender(current)))
    msg <- c(msg,"Senders do not match.")
  if (!(length(mess(target))==0L && length(mess(current))==0L) &&
      any(mess(target) != mess(current)))
    msg <- c(msg,"Messages do not match.")
  ## Check Data
  namet <- names(target@data)
  namec <- names(current@data)
  if (length(target@data) != length(current@data) ||
      !setequal(namet,namec)) {
    msg <- c(msg,"Names or number of data differ.")
    if (length(setdiff(namet,namec)) > 0L)
      msg <- c(msg,paste("Data in target but not in current:",
                         setdiff(namet,namec)))
    if (length(setdiff(namec,namet)) > 0L)
      msg <- c(msg,paste("Data in current but not in target:",
                         setdiff(namec,namet)))
  }
  msgd <- all.equal(target@data,current@data,...)
  if (!isTRUE(msgd)) msg <- c(msg,msgd)
  ## Timestamp
  if (checkTimestamp) {
    if (abs(timestamp(target)-timestamp(current)) >
        as.difftime(.1,units="secs"))
      msg <- c(msg,"Timestamps differ by more than .1 secs")
  }

  ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}

###
## This is a construction I find myself using in a lot of places to
## build up the "mongodb://" URI for the database.



#' Creates the URI needed to connect to a mongo database.
#' 
#' 
#' This function formats the universal record indicator (URI) for connecting to
#' a Mongo database.  It is mostly a utility function for formatting the
#' string.
#' 
#' 
#' @param username The name of the database user (login credential), or an
#' empty string if no username is required.
#' @param password The name of the database password (login credential), or an
#' empty string if no password is required.
#' @param host The name or IP address of the system hosting the database.
#' @param port The port to be used for connections.  Note that the port for a
#' default configuration of mongo is 27018.  This can be left blank to use the
#' default port.
#' @param protocol A character scalar giving the protocol to use when
#' connecting, e.g., \dQuote{mongodb}.
#' @return
#' 
#' A character string giving the database URI which can be passed to the
#' \code{\link[mongolite]{mongo}} function to create a database collection
#' handle.
#' 
#' Note that the password is stored in clear text, so appropriate care should
#' be taken with the result of this function.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{MongoDB}}, \code{\link[mongolite]{mongo}}
#' 
#' This is an input argument to a number of other classes which use mongo
#' connections.
#' @keywords interface database
#' @examples
#' 
#' 
#' stopifnot(makeDBuri()=="mongodb://localhost")
#' 
#' stopifnot(makeDBuri(user="admin",password="secret")==
#'                     "mongodb://admin:secret@localhost")
#' ## No password
#' stopifnot(makeDBuri(user="admin")==
#'                     "mongodb://admin@localhost")
#' 
#' stopifnot(makeDBuri(host="example.com",port=12345) ==
#'           "mongodb://example.com:12345")
#' 
#' 
#' @export makeDBuri
makeDBuri <- function(username="",password="", host="localhost",
                      port="",protocol="mongodb") {
  ## Setup DB URI
  security <- ""
  if (nchar(username) > 0L) {
    if (!is.null(password) && nchar(password) > 0L)
      security <- paste(username,password,sep=":")
    else
      security <- username
  }
  if (nchar(port) > 0L)
    host <- paste(host,port,sep=":")
  else
    host <- host
  if (nchar(security) > 0L)
    host <- paste(security,host,sep="@")
  paste(paste(protocol,":/",sep=""),host,sep="/")
}
