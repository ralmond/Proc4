################
## Abstract class containing a collection of evidence.

MessageQueue <-
  setRefClass("MessageQueue",
              c(app="character"),
              methods=list(
                  initialize = function(app=character(),...)
                    callSuper(app=app,...)
                  ))

MongoQueue <-
setRefClass("MongoQueue",
            c(messDB="JSONDB",
              builder="function"),
            contains="MessageQueue",
            methods=list(
                initialize=function(app,messDB=NULL,
                                    builder=buildMessage,...) {
                  if (is.null(messDB))
                    stop("Evidence database must be supplied for MongoQueue.")
                  callSuper(app=app,messDB=messDB,builder=builder,...)
                },
                queue=function() {
                  messDB
                },
                fetchNextMessage= function() {
                  getOneRec(messDB,
                            buildJQuery(app=app,processed=FALSE),
                            builder,
                            sort = c(timestamp = 1))

                },
                buildIndex = function() {
                  mdbIndex(messDB, add=
                                     buildJQuery(list(app=1,
                                                      processed=1,
                                                      timestamp=1)))
                }
            ))


ListQueue <-
setRefClass("ListQueue",
            c(messages="list",
              pos="integer"),
            contains="MessageQueue",
            methods = list(
                initialize=function(app,messages=list(),...) {
                  callSuper(app=app,messages=messages,pos=1L,...)
                },
                getCurrent=function() {
                  if (is.na(pos) || pos < 1L || pos>length(messages))
                    return(NULL)
                  messages[[pos]]
                },
                setCurrent=function(newmess) {
                  if (is.na(pos) || pos < 1L || pos>length(messages))
                    return(newmess)
                  messages[[pos]] <<- newmess
                  newmess
                },
                hasNext=function() {
                  return(!is.na(pos) && pos >= 1L &&
                         pos < length(messages))
                },
                nextMessage=function() {
                  pos <<- pos+1L
                  getCurrent()
                },
                reset=function() {
                  pos <<- 1L
                },
                fetchNextMessage=function() {
                  mes <- getCurrent()
                  while (!is.null(mes) && processed(mes)) {
                    mes <- nextMessage()
                  }
                  mes
                }
            ))

fetchNextMessage_ <- function() {}
setGeneric("fetchNextMessage",
           function (queue) standardGeneric("fetchNextMessage"))

setMethod("fetchNextMessage","MessageQueue",
          function (queue) {
            queue$fetchNextMessage()
          })



markAsProcessed_ <- function() {}
setMethod("markAsProcessed","MongoQueue",
          function(col,mess) markAsProcessed(col$queue(),mess))

## Is it safe to assume the current message is the one we are working on?
setMethod("markAsProcessed","ListQueue",
          function(col,mess) {
            mess <- markAsProcessed(NULL,mess)
            col$setCurrent(mess)
            mess
          })

markAsError_ <- function() {}
setMethod("markAsError","MongoQueue",
          function(col,mess,e) markAsError(col$queue(),mess,e))

setMethod("markAsError","ListQueue",
          function(col,mess,e) {
            mess <- markAsError(NULL,mess,e)
          })

## Moving this here from Runners, as doRunRun is getting too long.

cleanMessageQueue_ <- function() {}
setGeneric("cleanMessageQueue",
           function (queue,query,appid) StandardGeneric("cleanMessageQueue"))

setMethod("cleanMessageQueue", "MongoQueue",
          function (queue,query,appid){
            flog.debug("Removing old messages.")
            status <- withFlogging({
              if (!is.null(names(query))) {
                ##Single query make it multiple.
                remquery <- list(query)
              }
              for (rq in query) {
                flog.trace("RQ %s: %s",names(rq),rq)
                rquery <- do.call(buildJQuery,c(list(app=appid),rq))
                flog.trace("Removing %s",rquery)
                mdbRemove(queue$queue(),rquery)
              }
            }, context=sprintf("removing old messages: %s.",
                               paste(query,collapse=", ")))
            if (is(status,'try-error')) {
              flog.fatal("Error during database message removal: %s.",
                         status)
              stop(status)
            }
          })


importMessages_ <- function() {}
setGeneric("importMessages",
  function(queue,filelist,data.dir)
    stadardGeneric("importMessages"))


setMethod("importMessages","MongoQueue",
           function(queue,filelist,data.dir) {
             colname <- queue$queue()$collection
             dbname <- queue$queue()$db
             dburi <- queue$queue()$url
             for (fil in filelist) {
               impf <- file.path(data.dir,fil)
               if (!file.exists(impf)) {
                 flog.warn("File %s does not exist, skipping import.",
                           fil)
               } else {
                 status <-
                   system2("mongoimport",
                           c("--jsonArray",
                             "--uri","dburi",
                             "-d",dbname,
                             "-c",colname,
                             impf),
                           stdout=TRUE, stderr=TRUE)
                 if(!is.null(attr(status,"status"))) {
                   flog.fatal("Got error when loading import file.")
                   flog.fatal("Error:",status,capture=TRUE)
                   stop(status)
                 }
               }
             }
           })

resetProcessedMessages_ <- function() {}
setGeneric("resetProcessedMessages",
           function (queue,repquery) StandardGeneric("resetProcessedMessages"))

setMethod("resetProcessedMessages", "MongoQueue",

          function (queue,repquery){
            flog.debug("Clearing processed flags.")
            status <- withFlogging({
              if (!is.null(names(repquery))) {
                ## Single query make it multiple.
                repquery <- list(repquery)
              }
              for (rq in repquery) {
                rquery <- do.call(buildJQuery, c(list(app=queue$appid),rq))
                flog.trace("Reprocessing %s",rquery)
                mdbUpdate(queue$queue(),rquery,
                          '{"$set":{"processed":false}}',
                          multiple=TRUE)
              }
            }, context=sprintf("Clearing Processed Flag: %s.",
                               paste(repquery,collapse=", ")))
            if (is(status,'try-error')) {
              flog.fatal("Error while clearing processed: %s.",
                         status)
              stop(status)
            }
          })

setMethod("resetProcessedMessages", "ListQueue",
          function (queue,repquery){
            ## For now, ignore the filter.
            queue$queue <- sapply(queue$queue,
                                  function (mes)
                                    processed(mess) <- FALSE
                                  )
          })


