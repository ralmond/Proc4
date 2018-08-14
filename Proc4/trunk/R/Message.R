###  Message -- A message send around the P4 system.

setClass("P4Message",
         slots=c("_id"="character",    #Mongo ID
                 app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="character",    #Task or other context ID
                 sender="character",      #Which process sent the message
                 mess="character",      #Action Identifier
                 timestamp="POSIXt",      #When action took place.
                 data="list"              #More details.
                 ))
setGeneric("app",function(x) standardGeneric("app"))
setMethod("app","P4Message", function(x) x@app)
setGeneric("uid",function(x) standardGeneric("uid"))
setMethod("uid","P4Message", function(x) x@uid)
setGeneric("mess",function(x) standardGeneric("mess"))
setMethod("mess","P4Message", function(x) x@mess)
setGeneric("context",function(x) standardGeneric("context"))
setMethod("context","P4Message", function(x) x@context)
setGeneric("timestamp",function(x) standardGeneric("timestamp"))
setMethod("timestamp","P4Message", function(x) x@timestamp)
setGeneric("details",function(x) standardGeneric("details"))
setMethod("details","P4Message", function(x) x@data)

P4Message <- function(uid,context,sender,mess,timestamp=Sys.time(),
                        details=list(),app="default") {
  new("P4Message",app=app,uid=uid,context=context,sender=sender,
      mess=mess, timestamp=timestamp,data=details,"_id"=NA_character_)
}

setMethod("toString","P4Message", function(x, ...) {
  paste('P4Message:{ uid:',x@uid,', context:',x@context,
        ',', x@sender, "says:", x@mess, '}')
})
setMethod("show","P4Message",function(object) {
  cat(toString(object),"\n")
})




setGeneric("as.json",function(x) standardGeneric("as.json"))
setMethod("as.json","P4Message", function(x) {
  p4l <- attributes(x)
  p4l$"_id" <- NULL
  p4l$class <-NULL
  raw <- toJSON(p4l,auto_unbox=TRUE,POSIXt="mongo")
  ## Timestamp is not unboxed.  Need to do that manually.
  sub('"timestamp":\\[(.*)\\]','"timestamp":\\1',raw)
  })

saveMess <- function (mess, col) {
  if (is.na(mess@"_id")) {
    ## Insert
    jso <- as.json(mess)
    col$insert(jso)
    it <- col$iterate(jso,'{"_id":true}',limit=1)
    mess@"_id" <- it$one()$"_id"
  } else {
    ## Replace
    col$update(paste('{"_id":{"$oid":"',mess@"_id",'"}}',sep=""),
               paste('{"$set":',as.json(mess),'}',sep=""))
  }
  mess
}


getMESSbyID <- function(id,col) {
  it <- col$iterate(paste('{"_id":{"$oid":"',id,'"}}',sep=""),
                    '{}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseMessage(rec)
}



parseMessage<- function (rec) {
  new("P4Message","_id"=rec$"_id", app=rec$app, uid=rec$uid,
      context=rec$context,sender=rec$sender,mess=rec$mess,
      timestamp=rec$timestamp,data=parseData(rec$data))
}

parseData <- function (obsData) {
  ##Not sure if we need further processing.
  obsData
}

buildMessQuery <- function (uid,context,sender,mess,before,after,
                          timestamp=NULL,seqno=NA_integer_,
                          app="default") {
  query <- '{'
  ## app
  if (length(app)> 1L) {
    app <- paste('{"$in":',toJSON(app),'}',sep="")
  }
  query <- paste(query,'"app":"',app,'"',sep="")
  ## uid
  if (missing(uid)) {
    stop("Must specify user/student id.")
  }
  if (length(uid) > 1L) {
    uid <- paste('{"$in":',toJSON(uid),'}',sep="")
  } else {
    uid <- toJSON(uid,auto_unbox=TRUE)
  }
  query <- paste(query,', "uid":',uid,sep="")
  ## Context
  if (!missing(context)) {
    if (length(context) > 1L) {
      context <- paste('{"$in":',toJSON(context),'}',sep="")
    } else {
      context <- toJSON(context,auto_unbox=TRUE)
    }
    query <- paste(query,', "context":',context,sep="")
  }
  ## Sender
  if (!missing(sender)) {
    if (length(sender) > 1L) {
      sender <- paste('{"$in":',toJSON(sender),'}',sep="")
    } else {
      sender <- toJSON(sender,auto_unbox=TRUE)
    }
    query <- paste(query,', "sender":',sender,sep="")
  }
  ## Mess(age)
  if (!missing(mess)) {
    if (length(mess) > 1L) {
      mess <- paste('{"$in":',toJSON(mess),'}',sep="")
    } else {
      mess <- toJSON(mess,auto_unbox=TRUE)
    }
    query <- paste(query,', "mess":',mess,sep="")
  }
  ## Timestamp -- can be either single time or before or after range.
  if (!missing(timestamp)) {
    timestamp <- paste('"$in:"',toJSON(timestamp,POSIXt="mongo"))
  } else {
    lt <- NULL
    if (!missing(before) && is(before,"POSIXt")) {
      if (length(before) > 1L) {
        stop("Before must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      lt <- paste('"$lt":',toJSON(unbox(before),POSIXt="mongo"))
    }
    gt <- NULL
    if (!missing(after) && is(after,"POSIXt")) {
      if (length(after) > 1L) {
        stop("After must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      gt <- paste('"$gt":',toJSON(unbox(after),POSIXt="mongo"))
    }
    timestamp <- paste(c(lt,gt),collapse=",")
  }
  if (nchar(timestamp) > 0L) {
    query <- paste(query,', "timestamp":{',timestamp,'}',sep="")
  }

  #### Return as string.
  paste(query,'}',sep="")
}


getMESSone <- function(query,col) {
  it <- col$iterate(query,'{}',sort='{"timestamp":-1}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseMessage(rec)
}

getMESSmany <- function(query,sort=-1,col) {
  n <- col$count(query)
  result <- vector("list",n)
  it <- col$iterate(query,'{}',
                    sort=paste('{"timestamp":',sort,'}',sep=""))
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- parseMessage(rec)
    nn <- nn +1
  }
  result
}

