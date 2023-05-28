
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
                  receiveMessage = function (mess) {
                      flog.debug("Updating record for %s: %s",uid(mess),toString(mess))
                      flog.debug(".. from %s",sender(mess))
                      flog.trace("Message:",x=as.jlist(mess,attributes(mess)),
                                 capture=TRUE)
                      m_id(mess) <- NA_character_
                      query <- lapply(qfields,function(f) do.call(f,list(mess)))
                      names(query) <- qfields
                      mdbReplace(db,do.call(buildJQuery,query),
                                 as.json(mess,serialize=TRUE),upsert=TRUE)
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


