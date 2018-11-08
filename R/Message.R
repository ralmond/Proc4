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
setGeneric("as.jlist",function(obj,ml) standardGeneric("as.jlist"))

setMethod("as.json","P4Message", function(x) {
  jlist <- as.jlist(x,attributes(x))
  toJSON(jlist,POSIXt="mongo")
})

setMethod("as.jlist",c("P4Message","list"), function(obj,ml) {
  ml$"_id" <- NULL
  ml$class <-NULL
  ## Use manual unboxing for finer control.
  ml$app <- unbox(ml$app)
  ml$uid <- unbox(ml$uid)
  if (!is.null(ml$context) && length(ml$context)==1L)
    ml$context <- unbox(ml$context)
  if (!is.null(ml$sender) && length(ml$sender)==1L)
    ml$sender <- unbox(ml$sender)
  if (!is.null(ml$mess) && length(ml$mess)==1L)
    ml$mess <- unbox(ml$mess)
  ml$timestamp <- unbox(ml$timestamp) # Auto_unbox bug.
  ## Saves name data
  ml$data <- lapply(ml$data,
                      function (s) lapply(s,unbox)) #Saves name data.
  ml
  })

saveRec <- function (mess, col) {
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


parseMessage<- function (rec) {
  new("P4Message","_id"=rec$"_id", app=rec$app, uid=rec$uid,
      context=rec$context,sender=rec$sender,mess=rec$mess,
      timestamp=rec$timestamp,data=parseData(rec$data))
}


## Maybe I can use the jsonlite::serializeJSON, unserializeJSON
## functions here to handle more cases than I'm currently handling.
parseData <- function (messData) {
  ##Need to convert back from list to numeric/character
  for (i in 1:length(messData)) {
    datum <- messData[[i]]
    if (all(sapply(datum,is.character))) {
      datum <- as.character(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.numeric))) {
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

mongoQueries <- c("eq","gt","gte","lt","lte","ne","nin","in","","oid")

#### Need to override jsonlite::unbox as it doesn't properly handle POSIXt objects.
unbox <- function (x) {
  if (length(x) == 1L && is(x,"POSIXt")) {
    jsonlite:::as.scalar(x)
  } else {
    jsonlite::unbox(x)
  }
}

buildJQterm <- function (name,value) {
  if (length(value)==0L)
    stop("Query term ",name,"has no value.")
  compOps <- names(value)
  names(value) <- NULL
  if (is.null(compOps)) {
    if (length(value) == 1L) {
      ## Singleton query.
      vstring <- toJSON(unbox(value),POSIXt="mongo")
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
                                toJSON(unbox(value[i]),POSIXt="mongo"),
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


getOneRec <- function(jquery,col,parser) {
  it <- col$iterate(jquery,'{}',sort='{"timestamp":-1}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  do.call(parser,list(rec))
}

getManyRecs <- function(jquery,col,parser,sort=c("timestamp"=-1)) {
  n <- col$count(jquery)
  result <- vector("list",n)
  it <- col$iterate(query,'{}',
                    sort=paste('{"timestamp":',sort,'}',sep=""))
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- do.call(parser,list(rec))
    nn <- nn +1
  }
  result
}

