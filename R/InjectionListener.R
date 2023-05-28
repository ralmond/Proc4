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
                  receiveMessage = function (mess) {
                      flog.debug("Sending message %s",toString(mess))
                      flog.debug(".. from %s",sender(mess))
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mongo::m_id(mess) <- NA_character_
                      mdbInsert(messdb(),as.json(mess,serialize=TRUE))
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


