
#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.

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


