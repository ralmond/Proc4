
#################################################
## UpsertListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

UpsertListener <-
  setRefClass("UpsertListener",
              fields=c(name="character",
                       sender="character",
                       qfields="character",
                       messSet = "character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Upsert",
                             sender="sender",
                             dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             messSet=character(),
                             noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                             mongoverbose=FALSE,
                             ssl_options=mongolite::ssl_options(),
                             qfields=c("app","uid"),
                             ...) {
                      messDB <- mongo::MongoDB(colname,dbname,dburi,
                                              verbose=mongoverbose,
                                              noMongo=noMongo,
                                              options=ssl_options)
                      callSuper(name=name,sender=sender,db=messDB,
                                messSet=messSet,
                                qfields=qfields,
                                ...)
                    },
                  messdb = function () {
                    db
                  },
                  receiveMessage = function (mess) {
                    if (mess(mess) %in% messSet) {
                      flog.debug("Updating record for %s: %s",uid(mess),toString(mess))
                      flog.debug(".. from %s",sender)
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      mess@sender <- sender
                      mess@"_id" <- NA_character_
                      query <- lapply(qfields,function(f) do.call(f,list(mess)))
                      names(query) <- qfields
                      mdbReplace(db,do.call(buildJQuery,query),
                                 as.json(mess,serialize=TRUE),upsert=TRUE)
                    } else {
                      flog.debug("%s ignoring message %s",toString(sender),
                                 toString(mess))
                    }
                  },
                  reset = function(app) {
                    mdbRemove(db,buildJQuery(app=app))
                  }
              ))

UpsertListener <- function (name="Upsert",
                            sender="sender",
                            dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                            mongoverbose=FALSE,
                            ssl_options=mongolite::ssl_options(),
                            qfields=c("app","uid"),...) {
  new("UpsertListener",name=name,sender=sender,dbname=dbname,dburi=dburi,
      colname=colname,noMongo=noMongo,mongoverbose=mongoverbose,
      ssl_options=ssl_options,
      messSet=messSet,qfields=qfields,...)
}

setMethod("isListener","UpsertListener",function(x) TRUE)
setMethod("receiveMessage","UpsertListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","UpsertListener",function(x) x$name)

