#################################################
## InjectionListener

## This a simple listener whose goal is to simply to inject the
## message into a mongo collection where it can be used as a queue.

InjectionListener <-
  setRefClass("InjectionListener",
              list(),
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
                  messdb = function() {db},
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


setMethod("listenerDataTable","InjectionListener",
          function(listener,appid=character()) {
            stat1 <- mdbFind(listener$messdb(),buildJQuery(app=appid))
            if (isTRUE(nrow(stat1) > 0L)) {
              stat1data <- t(sapply(stat1$data,parseData))
              sdat <- data.frame(stat1[,c("app","uid","context","timestamp")],
                                 stat1data)
              sdat$app <- basename(sdat$app)
              return(sdat)
            } else {
              flog.warn("No records in statistics file.")
              return(NULL)
            }
          })

## These are left here as they may be useful later; however, they are
buildHistMat <- function (col, app, uid) {
  stat1 <- mdbFind(col,buildJQuery(app=app, uid=uid))
  stat1d <- lapply(stat1$data,function (d) Peanut::flattenStats(parseData(d)))
  data.frame(stat1[,c("app","uid","context","timestamp")],
             do.call(rbind,stat1d))
}

buildAppHist <- function (col, app) {
  uids <- col$distinct("uid",buildJQuery(app=app))
  do.call(rbind,
          lapply(uids,function(u) buildHistMat(col, app, u)))
}
