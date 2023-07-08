##############################################
## Table Listener -- Based on work by Lukas Liu and Nan Wang

TableListener <-
  setRefClass("TableListener",
              fields=list(fieldlist = "character",  #Named list of types
                          df = "data.frame"
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
                },
                reset = function(app) {
                    initDF()
                },
                returnDF = function() {
                  ## deletes the first row of DF because initDF()
                  ## creates an empty row
                  df[-1, ]
                }
              ),
              contains="RefListener")


TableListener <- function (name="ppData",
                             fieldlist=c("uid"="character",
                                         "context"="character"),
                             messSet=character(),...) {
  new("TableListener",name=name,fieldlist=fieldlist,messSet=messSet,...)
}

setMethod("listenerDataTable","TableListener",
          function(listener, appid=character())
            listener$returnDF()
)

