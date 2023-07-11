serializeData <- function(jlist) {
  encodeString(jsonlite::serializeJSON(jlist),quote='"')
}


#################################################
## UpdateListener

## This listener updates fields in a reference database for a given
## listener.  It can be used, for example, to keep track of trophies
## earned or balance of money/earned or spent in the game.

UpdateListener <-
  setRefClass("UpdateListener",
              fields=c(qfields="character",
                       targetField="character",
                       jsonEncoder="ANY",
                       jsonDecoder="ANY"),
              methods=list(
                  initialize=
                    function(name="Update",
                             db=MongoDB(noMongo=TRUE),
                             messSet=character(),
                             targetField="data",
                             jsonEncoder="serializeData",
                             jsonDecoder="unserializeData",
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(name=name,db=db,
                                messSet=messSet,
                                targetField=targetField,
                                jsonEncoder=jsonEncoder,
                                qfields=qfields,
                                ...)
                    },
                  receiveMessage = function (message) {
                    flog.debug("Updating record for %s (%s): %s",uid(message),
                               context(message), toString(message))
                    if (nchar(targetField) > 0L) {
                      encoded <- do.call(jsonEncoder,list(details(message)))
                      flog.trace("New Data Value:",encoded,capture=TRUE)
                      update <- sprintf('{"$set":{"%s":%s, "context":"%s", "timestamp":%s}}',
                                        targetField,
                                        encodeString(encoded,quote='"'),
                                        context(message),
                                        jsonlite::toJSON(unboxer(timestamp(message)), POSIXt="mongo"))
                    } else {
                      update <- sprintf('{"$set":%s}',
                                        do.call(jsonEncoder,
                                                list(details(message))))
                    }
                    query <- lapply(qfields,function(f) do.call(f,list(message)))
                    names(query) <- qfields
                    qq <- do.call(buildJQuery,query)
                    count <- mdbCount(db,qq)
                    if (is.na(count)) {
                      flog.debug("Not saving record, database connection is invalid.")
                    } else if (count == 0L) {
                      ## Initializize by saving message.
                      flog.trace("Record not found, inserting.")
                      m_id(message) <- NA_character_
                      mdbInsert(db,as.json(message))
                    } else {
                      flog.trace("Record found, updating.")
                    }
                    ## Insert does not format details, correctly.
                    ## Overwrite with update.
                    flog.trace("Update: %s",update)
                    mdbUpdate(db,qq,update)
                  },
                  reset = function(app) {
                    mdbRemove(db,buildJQuery(app=app))
                  }

              ), contains="RefListener")

UpdateListener <- function (name="Update",
                            db=mongo::MongoDB(noMongo=TRUE),
                            targetField="data",
                            qfields=c("app","uid"),
                            jsonEncoder="unparseData",
                            jsonDecoder="parseData",
                            messSet=character(),
                            ...) {
  new("UpdateListener",name=name,db=db,messSet=messSet,
      targetField=targetField, jsonEncoder=jsonEncoder,
      jsonDecoder=jsonDecoder, qfields=qfields,...)
}

## TODO: Fix this method.
setMethod("listenerDataTable","UpdateListener",
          function(listener,appid=character()) {
            stat1 <- mdbFind(listener$messdb(),buildJQuery(app=appid))
            if (isTRUE(nrow(stat1) > 0L)) {
              if (nchar(listener$targetField) > 0L) {
                fielddat <- t(sapply(stat1[[listener$targetField]],listener$jsonDecoder))
                sdat <- data.frame(stat1[,c("app","uid","context","timestamp")],
                                   fielddat)
              } else {
                sdat <- stat1
              }
              sdat$app <- basename(sdat$app)
              return(sdat)
            } else {
              flog.warn("No records in statistics file.")
              return(NULL)
            }
          })



