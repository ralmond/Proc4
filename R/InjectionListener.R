#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

InjectionListener <-
  setRefClass("InjectionListener",
              fields=c(name = "character",
                       sender="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Injection",
                             sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                             mongoverbose=FALSE,
                             ssl_options=mongolite::ssl_options(),
                             messSet=character(),
                             ...) {
                      messDB <- mongo::MongoDB(colname,dbname,dburi,
                                              verbose=mongoverbose,
                                              noMongo=noMongo,
                                              options=ssl_options)
                      callSuper(name=name,sender=sender,db=messDB,
                                messSet=messSet,
                                ...)
                    },
                  messdb = function () {
                    db
                  },
                  receiveMessage = function (mess) {
                    if (mess(mess) %in% messSet) {
                      flog.debug("Sending message %s",toString(mess))
                      flog.debug(".. from %s",sender)
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mess@sender <- sender
                      mess@"_id" <- NA_character_
                      mdbInsert(messdb(),as.json(mess,serialize=TRUE))
                    } else {
                      flog.debug("%s ignoring message %s",toString(sender),
                                 toString(mess))
                    }
                  },
                  reset = function(app) {
                    if (!is.null(messdb()))
                      mdbRemove(messdb(),buildJQuery(app=app))
                  }
              ))

InjectionListener <- function (name="Injection",
                               sender="sender",
                               dbname="test",
                               dburi="mongodb://localhost",
                               messSet=character(),
                               colname="Messages",
                               noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                               mongoverbose=FALSE,
                               ssl_options=mongolite::ssl_options(),
                               ...) {
  new("InjectionListener",name=name,sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,noMongo=noMongo,mongoverbose=mongoverbose,
      ssl_options=ssl_optoins,messSet=messSet,...)
}

setMethod("isListener","InjectionListener",function(x) TRUE)
setMethod("receiveMessage","InjectionListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","InjectionListener",function(x) x$name)
