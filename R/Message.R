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
         slots=c(app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="character",    #Task or other context ID
                 sender="character",      #Which process sent the message
                 mess="character",      #Action Identifier
                 timestamp="POSIXt",      #When action took place.
                 processed="logical",     #Has this message been processed by the reciever.
                 pError="ANY",     #Error occured while processing.
                 data="list"              #More details.
                 ),
         contains="MongoRec")

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
  callNextMethod(obj,ml,serialize)
  })



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

