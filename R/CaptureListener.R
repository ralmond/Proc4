
#################################################
## CaptureListener

## This a simple listener whose goal is to simply hold the message to
## it can be checked later.

CaptureListener <-
  setRefClass("CaptureListener",
              fields=c(messages="list"),
              methods=list(
                  initialize = function(name="Capture",messages=list(),...)
                    callSuper(name=name,messages=messages,...),
                  receiveMessage = function (mess) {
                        messages <<- c(mess,messages)
                  },
                  lastMessage = function() {
                    if(length(messages)==0L) return(NULL)
                    messages[[1]]
                  },
                  reset = function(app) {
                    messages <<- list()
                  }),
                  contains="RefListener")

CaptureListener <- function (name="Capture",messages=list(),messSet=character(),...) {
  new("CaptureListener",name=name,messages=messages,messSet=messSet)
}




