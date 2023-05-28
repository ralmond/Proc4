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
                       jsonEncoder="ANY"),
              methods=list(
                  initialize=
                    function(name="Update",
                             db=MongoDB(noMongo=TRUE),
                             messSet=character(),
                             targetField="data",
                             jsonEncoder="serializeData",
                             qfields=c("app","uid"),
                             ...) {
                      callSuper(name=name,db=db,
                                messSet=messSet,
                                targetField=targetField,
                                jsonEncoder=jsonEncoder,
                                qfields=qfields,
                                ...)
                    },
                  receiveMessage = function (mess) {
                    flog.debug("Updating record for %s (%s): %s",uid(mess),
                               context(mess), toString(mess))
                    if (nchar(targetField) > 0L) {
                      encoded <- do.call(jsonEncoder,list(details(mess)))
                      flog.trace("New Data Value:",encoded,capture=TRUE)
                      update <- sprintf('{"$set":{"%s":%s, "context":"%s", "timestamp":%s}}',
                                        targetField,
                                        encodeString(encoded,quote='"'),
                                        context(mess),
                                        jsonlite::toJSON(unboxer(timestamp(mess)), POSIXt="mongo"))
                    } else {
                      update <- sprintf('{"$set":%s}',
                                        do.call(jsonEncoder,
                                                list(details(mess))))
                    }
                    query <- lapply(qfields,function(f) do.call(f,list(mess)))
                    names(query) <- qfields
                    qq <- do.call(buildJQuery,query)
                    if (mdbCount(db,qq) == 0L) {
                      ## Initializize by saving message.
                      flog.trace("Record not found, inserting.")
                      mess@"_id" <- NA_character_
                      mdbInsert(db,as.json(mess))
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
                            messSet=character(),
                            ...) {
  new("UpdateListener",name=name,db=db,messSet=messSet,
      targetField=targetField,jsonEncoder=jsonEncoder,
      qfields=qfields,...)
}



