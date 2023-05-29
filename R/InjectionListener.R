#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

InjectionListener <-
  setRefClass("InjectionListener",
              fields=character(),
              methods=list(
                  initialize=
                    function(name="Injection",
                             db=mongo::MongoDB(noMongo=TRUE),
                             messSet=character(),
                             ...) {
                      callSuper(name=name,db=db,
                                messSet=messSet,
                                ...)
                    },
                  receiveMessage = function (message) {
                      flog.debug("Sending message %s",toString(message))
                      flog.debug(".. from %s",sender(message))
                      flog.trace("Message:",x=as.jlist(message,
                                                       attributes(message)),
                                 capture=TRUE)
                      mongo::m_id(message) <- NA_character_
                      mdbInsert(messdb(),as.json(message,serialize=TRUE))
                  },
                  reset = function(app) {
                    if (!is.null(messdb()))
                      mdbRemove(messdb(),buildJQuery(app=app))
                  }
              ),
              contains="RefListener")

InjectionListener <- function (name="Injection",
                               db=mongo::MongoDB(noMongo=TRUE),
                               messSet=character(),
                               ...) {
  new("InjectionListener",name=name,db=db,messSet=messSet,...)
}


