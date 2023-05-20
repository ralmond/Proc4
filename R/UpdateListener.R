
#################################################
## UpdateListener

## This listener updates fields in a reference database for a given
## listener.  It can be used, for example, to keep track of trophies
## earned or balance of money/earned or spent in the game.

UpdateListener <-
  setRefClass("UpdateListener",
              fields=c(name="character",
                       messSet = "character",
                       qfields="character",
                       targetField="character",
                       jsonEncoder="character",
                       db="MongoDB"),
              methods=list(
                  initialize=
                    function(name="Update",dbname="test",
                             dburi="mongodb://localhost",
                             colname="Messages",
                             noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                             mongoverbose=FALSE,
                             ssl_options=mongolite::ssl_options(),
                             messSet=character(),
                             targetField="data",
                             jsonEncoder="unparseData",
                             qfields=c("app","uid"),
                             ...) {
                      messDB <- mongo::MongoDB(colname,dbname,dburi,
                                              verbose=mongoverbose,
                                              noMongo=noMongo,
                                              options=ssl_options)
                      callSuper(name=name,db=messDB,
                                messSet=messSet,
                                targetField=targetField,
                                jsonEncoder=jsonEncoder,
                                qfields=qfields,
                                ...)
                    },
                  messdb = function () {
                    db
                  },
                  receiveMessage = function (mess) {
                    if (mess(mess) %in% messSet) {
                      flog.debug("Updating record for %s (%s): %s",uid(mess),
                                 context(mess), toString(mess))
                      if (nchar(targetField) > 0L) {
                        update <- sprintf('{"$set":{"%s":%s, "context":"%s", "timestamp":%s}}',
                                          targetField,
                                          do.call(jsonEncoder,
                                                  list(details(mess))),
                                          context(mess),
                                          toJSON(unboxer(timestamp(mess)),
                                                 POSIXt="mongo"))
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
                    } else {
                      flog.debug("%s ignoring message %s",dbname,toString(mess))
                    }
                  },
                  reset = function(app) {
                    mdbRemove(db,buildJQuery(app=app))
                  }

              ))

UpdateListener <- function (name="Update",dbname="test",
                            dburi="mongodb://localhost",
                            messSet=character(),
                            colname="Messages",
                            noMongo=(length(dburi)==0L|nchar(dburi)=0L),
                            mongoverbose=FALSE,
                            ssl_options=mongolite::ssl_options(),
                            targetField="data",
                            qfields=c("app","uid"),
                            jsonEncoder="unparseData",
                            ...) {
  new("UpdateListener",name=name,dbname=dbname,dburi=dburi,messSet=messSet,
      colname=colname,noMongo=noMongo,mongoverbose=mongoverbose,
      ssl_options=ssl_options,
      targetField=targetField,jsonEncoder=jsonEncoder,
      qfields=qfields,...)
}

setMethod("isListener","UpdateListener",function(x) TRUE)
setMethod("receiveMessage","UpdateListener",
          function(x,mess) x$receiveMessage(mess))
setMethod("listenerName","UpdateListener",function(x) x$name)

