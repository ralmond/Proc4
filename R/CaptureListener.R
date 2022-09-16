
#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.
#' Class \code{"CaptureListener"}
#'
#'
#' This listener simply takes its messages and adds them to a list.  It is is
#' mainly used for testing the message system.
#'
#'
#' This listener simply takes all messages and pushes them onto the
#' \code{messages} field.  The \code{messages} field is the complete list of
#' received messages, most recent to most ancient.  The method
#' \code{lastMessage()} returns the most recent message.
#'
#' @name CaptureListener-class
#' @aliases CaptureListener-class isListener,CaptureListener-method
#' receiveMessage,CaptureListener-method listenerName,CaptureListener-method
#' @docType class
#' @section Extends:
#'
#' This class implements the \code{\link{Listener}} interface.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Russell Almond
#' @seealso \code{\link{Listener}}, \code{\linkS4class{P4Message}},
#' \code{\link{CaptureListener}}, \code{\linkS4class{UpdateListener}},
#' \code{\linkS4class{UpsertListener}}, \code{\linkS4class{InjectionListener}},
#' \code{\linkS4class{TableListener}},
#' @references
#'
#' This is an example of the observer design pattern.
#' \url{https://en.wikipedia.org/wiki/Observer_pattern}.
#' @keywords classes
#' @examples
#'
#'
#' mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
#'                    sender="EABN",mess="Statistics",
#'                    details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
#'
#' cl <- CaptureListener()
#' receiveMessage(cl,mess1)
#' stopifnot(all.equal(mess1,cl$lastMessage()))
#'
#'
CaptureListener <-
  setRefClass("CaptureListener",
              fields=c(name = "character",messages="list"),
              methods=list(
                  initialize = function(name="Capture",messages=list(),...)
                    callSuper(name=name,messages=messages,...),
                  receiveMessage = function (mess) {
                    messages <<- c(mess,messages)
                  },
                  lastMessage = function() {
                    messages[[1]]
                  },
                  reset = function(app) {
                    messages <<- list()
                  }))

CaptureListener <- function (name="Capture",messages=list(),...) {
  new("CaptureListener",name=name,messages=messages,...)
}

setMethod("isListener","CaptureListener",function(x) TRUE)
setMethod("receiveMessage","CaptureListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","CaptureListener",function(x) x$name)

