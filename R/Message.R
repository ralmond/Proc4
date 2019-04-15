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
setGeneric("uid",function(x) standardGeneric("uid"))
setGeneric("mess",function(x) standardGeneric("mess"))
setGeneric("context",function(x) standardGeneric("context"))
setGeneric("sender",function(x) standardGeneric("sender"))
setGeneric("timestamp",function(x) standardGeneric("timestamp"))
setGeneric("details",function(x) standardGeneric("details"))

setMethod("app","P4Message", function(x) x@app)
setMethod("uid","P4Message", function(x) x@uid)
setMethod("mess","P4Message", function(x) x@mess)
setMethod("context","P4Message", function(x) x@context)
setMethod("sender","P4Message", function(x) x@sender)
setMethod("timestamp","P4Message", function(x) x@timestamp)
setMethod("details","P4Message", function(x) x@data)

P4Message <- function(uid,context,sender,mess,timestamp=Sys.time(),
                        details=list(),app="default") {
  new("P4Message",app=app,uid=uid,context=context,sender=sender,
      mess=mess, timestamp=timestamp,data=details,
      "_id"=c(oid=NA_character_))
}

setMethod("toString","P4Message", function(x, ...) {
  paste('P4Message:{ uid:',x@uid,', context:',x@context,
        ',', x@sender, "says:", x@mess, '}')
})
setMethod("show","P4Message",function(object) {
  cat(toString(object),"\n")
})




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
  if (!is.null(ml$context) && length(ml$context)==1L)
    ml$context <- unboxer(ml$context)
  if (!is.null(ml$sender) && length(ml$sender)==1L)
    ml$sender <- unboxer(ml$sender)
  if (!is.null(ml$mess) && length(ml$mess)==1L)
    ml$mess <- unboxer(ml$mess)
  ml$timestamp <- unboxer(ml$timestamp) # Auto_unbox bug.
  ## Saves name data
  ml$data <- unparseData(ml$data,serialize)
  ml
  })

saveRec <- function (mess, col, serialize=TRUE) {
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
      ## ID is out of date, instert and get new ID.
      col$insert(jso)
      it <- col$iterate(jso,'{"_id":true}',limit=1)
      mess@"_id" <- it$one()$"_id"
      names(mess@"_id") <- "oid" ## Aids in extraction
    }
  }
  mess
}

## as.vector suppresses the names which are harmless, but make writing
## test suites harder.
parseMessage<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  new("P4Message","_id"=ununboxer(rec$"_id"),
      app=as.vector(ununboxer(rec$app)),
      uid=as.vector(ununboxer(rec$uid)),
      context=as.vector(ununboxer(rec$context)),
      sender=as.vector(ununboxer(rec$sender)),
      mess=as.vector(ununboxer(rec$mess)),
      timestamp=ununboxer(rec$timestamp),
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
      vstring <- paste('{"$in":',toJSON(value,POSIXt="mongo"),'}',sep="")
    }
  } else {
    if(!all(compOps %in% mongoQueries)) {
      stop("Unspported operator",compOps[!(compOps%in%mongoQueries)],
           "in query for field",name)
    }
    if(compOps[1]=="nin" || compOps[1]=="in" || compOps[1]=="") {
      ## Special Handling for (n)in query)
      op <- ifelse(compOps[1]=="","in",compOps[1])
      vstring <- paste('{"$',op,'":',toJSON(value,POSIXt="mongo"),'}',sep="")
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

buildJQuery <- function (...,rawfields=character()) {
  terms <- list(...)
  fields <- names(terms)
  jstrings <- sapply(fields,function(f) buildJQterm(f,terms[[f]]))
  jstrings <- c(jstrings,rawfields)
  paste('{',paste(jstrings,collapse=", "),'}')
}


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
        asif.difftime(list(secs=.1)))
      msg <- c(msg,"Timestamps differ by more than .1 secs")
  }

  ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}