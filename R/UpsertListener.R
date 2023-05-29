
#################################################
## UpsertListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

UpsertListener <-
  setRefClass("UpsertListener",
              fields=c(qfields="character"),
              methods=list(
                  initialize=
                    function(name="Upsert",
                             db=mongo::MongoDB(noMongo=TRUE),
                             qfields=c("app","uid"),messSet=character(),
                             ...) {
                      callSuper(name=name,db=db,
                                messSet=messSet,
                                qfields=qfields,
                                ...)
                    },
                  receiveMessage = function (message) {
                      flog.debug("Updating record for %s: %s",uid(message),toString(message))
                      flog.debug(".. from %s",sender(message))
                      flog.trace("Message:",x=as.jlist(message,attributes(message)),
                                 capture=TRUE)
                      m_id(message) <- NA_character_
                      query <- lapply(qfields,function(f) do.call(f,list(message)))
                      names(query) <- qfields
                      mdbReplace(db,do.call(buildJQuery,query),
                                 as.json(message,serialize=TRUE),upsert=TRUE)
                  },
                  reset = function(app) {
                    mdbRemove(db,buildJQuery(app=app))
                  }
              ), contains="RefListener")

UpsertListener <- function (name="Upsert",messSet=character(),
                            db=mongo::MongoDB(noMongo=TRUE),
                            qfields=c("app","uid"),...) {
  new("UpsertListener",name=name,db=db,
      messSet=messSet,qfields=qfields,...)
}


