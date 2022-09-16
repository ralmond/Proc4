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

