###  Message -- A message send around the P4 system.

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
setGeneric("app<-",function(x, value) standardGeneric("app<-"))
setGeneric("uid",function(x) standardGeneric("uid"))
setGeneric("uid<-",function(x, value) standardGeneric("uid<-"))
setGeneric("mess",function(x) standardGeneric("mess"))
setGeneric("mess<-",function(x, value) standardGeneric("mess<-"))
setGeneric("context",function(x) standardGeneric("context"))
setGeneric("context<-",function(x, value) standardGeneric("context<-"))
setGeneric("sender",function(x) standardGeneric("sender"))
setGeneric("sender<-",function(x, value) standardGeneric("sender<-"))
setGeneric("timestamp",function(x) standardGeneric("timestamp"))
setGeneric("timestamp<-",function(x, value) standardGeneric("timestamp<-"))
setGeneric("details",function(x) standardGeneric("details"))
setGeneric("details<-",function(x, value) standardGeneric("details<-"))
setGeneric("processed",function(x) standardGeneric("processed"))
setGeneric("processed<-",function(x, value) standardGeneric("processed<-"))
setGeneric("processingError",function(x) standardGeneric("processingError"))
setGeneric("processingError<-",function(x, value)
  standardGeneric("processingError<-"))

setMethod("app","P4Message", function(x) x@app)
setMethod("app<-","P4Message", function(x, value){
  x@app <- value
  x
})
setMethod("uid","P4Message", function(x) x@uid)
setMethod("uid<-","P4Message", function(x, value) {
  x@uid<-value
  x})
setMethod("mess","P4Message", function(x) x@mess)
setMethod("mess<-","P4Message", function(x, value) {
  x@mess<-value
  x})
setMethod("context","P4Message", function(x) x@context)
setMethod("context<-","P4Message", function(x, value) {
  x@context <- value
  x})
setMethod("sender","P4Message", function(x) x@sender)
setMethod("sender<-","P4Message", function(x, value) {
  x@sender <- value
  x})
setMethod("timestamp","P4Message", function(x) x@timestamp)
setMethod("timestamp<-","P4Message", function(x,value) {
  x@timestamp <- as.POSIXct(value)
  x})
setMethod("details","P4Message", function(x) x@data)
setMethod("details<-","P4Message", function(x, value) {
  x@data<-value
  x})
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

P4Message <- function(uid="",context="",sender="",mess="",timestamp=Sys.time(),
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



as_jlist <- function() {}

setMethod("as.jlist",c("P4Message","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
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

setGeneric("markAsProcessed",function(col,mess)
  standardGeneric("markAsProcessed"))
setMethod("markAsProcessed",c("JSONDB","P4Message"),
          function (col,mess) {
            processed(mess) <- TRUE
            mdbUpdate(col,paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
                      '{"$set": {"processed":true}}')
            mess
          })
setMethod("markAsProcessed",c("NULL","P4Message"),
          function(col,mess) {
            processed(mess) <- TRUE
            mess
            })


setGeneric("markAsError",function(col,mess,e) standardGeneric("markAsError"))
setMethod("markAsError", c("JSONDB","P4Message"),
           function (col,mess, e) {
             processingError(mess) <- e
             mdbUpdate(col,paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
                       sprintf('{"$set": {"pError":%s}}',
                               encodeString(toString(e),quote='"'))
                       )
             mess
           })

setMethod("markAsError", c("NULL","P4Message"),
           function (col,mess, e) {
             processingError(mess) <- e
             mess
           })




## as.vector suppresses the names which are harmless, but make writing
## test suites harder.

## The cleaning code gets reused by other classes which inherit from
## P4Message.
## toJSON | fromJSON on an empty list will change the type, so need to
## check for empty lists.
cleanMessageJlist <- function (rec) {
  rec$app <- as.character(ununboxer(rec$app))
  if (is.null(rec$app) || length(rec$app) == 0L) rec$app <- "default"
  rec$uid <- as.character(ununboxer(rec$uid))
  rec$context <- as.character(ununboxer(rec$context))
  if (is.null(rec$context) || length(rec$context) == 0L) rec$context <-""
  rec$context <- trimws(as.character(rec$context))
  rec$mess <- as.character(ununboxer(rec$mess))
  if (is.null(rec$mess) || length(rec$mess) == 0L) rec$mess <-""
  rec$mess <- trimws(as.character(rec$mess))
  rec$sender <- as.character(ununboxer(rec$sender))
  if (is.null(rec$sender)|| length(rec$sender) == 0L) rec$sender <-""
  rec$processed <- as.logical(ununboxer(rec$processed))
  if (is.null(rec$processed) || is.na(rec$processed) || length(rec$processed)==0L) rec$processed <- FALSE
  rec$timestamp <- ununboxer(rec$timestamp)
  if (is.null(rec$timestamp)) rec$timestamp <- Sys.time()
  if (is.list(rec$timestamp)) rec$timestamp <- rec$timestamp$`$date`
  rec$timestamp <- as.POSIXlt(rec$timestamp)
  rec$pError <- as.character(ununboxer(rec$pError))
  rec
}

setMethod("parse.jlist",c("P4Message","list"),
  function(class,rec) {
    rec <- cleanMessageJlist(rec)
    rec$data <- mongo::parseData(rec$data)
    callNextMethod(class,rec)
  })

buildMessage<- function (rec,class="P4Message") {
  jlp <- selectMethod("parse.jlist",c(class,"list"))
  rec <- do.call(jlp,list(class,rec))
  rec$class <- NULL
  do.call("new",c(class,rec))
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
