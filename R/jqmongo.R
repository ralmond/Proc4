
mongoQueries <- c("eq","gt","gte","lt","lte","ne","nin","in","","oid")


buildJQterm <- function (name,value) {
  if (length(value)==0L)
    stop("Query term ",name,"has no value.")
  compOps <- names(value)
  names(value) <- NULL
  if (is.null(compOps)) {
    if (length(value) == 1L) {
      ## Singleton query.
      vstring <- toJSON(unboxer(value),POSIXt="mongo")
    } else {
      ## Unmarked $in query
      vstring <- paste('{"$in":',toJSON(unlist(value),
                                        POSIXt="mongo"),'}',sep="")
    }
  } else {
    compOps <- sub('^\\$','',compOps)   #Strip leading $
    if(!all(compOps %in% mongoQueries)) {
      stop("Unspported operator ",compOps[!(compOps %in% mongoQueries)],
           " in query for field ",name)
    }
    if(compOps[1]=="nin" || compOps[1]=="in" || compOps[1]=="") {
      ## Special Handling for (n)in query)
      op <- ifelse(compOps[1]=="","in",compOps[1])
      vstring <- paste('{"$',op,'":',toJSON(unlist(value),
                                            POSIXt="mongo"),'}',sep="")
    } else {
      ## iterate over values.
      vstring <- sapply(1:length(compOps),
                        function (i)
                          paste('"$',compOps[i],'":',
                                toJSON(unboxer(value[i]),POSIXt="mongo"),
                                sep=""))
      vstring <- paste('{',paste(vstring,collapse=", "),'}')
    }
  }
  paste('"',name,'":',vstring,sep="")
}



#' Transforms a query into JQuery JSON.
#' 
#' 
#' This function takes a query which is expressed in the argument list and
#' transforms it into a JSON query document which can be used with the Mongo
#' Database.  The function \code{buildJQterm} is a helper function which builds
#' up a single term of the query.
#' 
#' 
#' A typical query to a Mongo database collection is done with a JSON object
#' which has a number of bits that look like
#' \dQuote{\emph{field}:\emph{value}}, where \emph{field} names a field in the
#' document, and \emph{value} is a value to be matched.  A record matches the
#' query if all of the fields specified in the query match the corresponding
#' fields in the record.
#' 
#' Note that \emph{value} could be a special expression which gives specifies a
#' more complex expression allowing for ranges of values.  In particular, the
#' Mongo query language supports the following operators: \code{"$eq", "$ne",
#' "$gt", "$lt", "$gte", "$lte"}.  These can be specified using a value of the
#' form \code{c(<op>=<value>)}, where \emph{op} is one of the mongo operators,
#' without the leading \sQuote{$}.  Multiple op--value pairs can be specified;
#' for example, \code{count=c(gt=3,lt=6)}.  If no op is specified, then
#' \code{"$eq"} is assumed.  Additionally, the \code{"$oid"} operator can be
#' used to specify that a value should be treated as a Mongo record identifier.
#' 
#' The \code{"$in"} and \code{"$nin"} are also ops, but the corrsponding value
#' is a vector.  They test if the record is in or not in the specified value.
#' If the value is vector valued, and no operator is specified it defaults to
#' \code{"$in"}.
#' 
#' The function \code{buildJQuery} processes each of its arguments, adding them
#' onto the query document.  The \code{rawfields} argument adds the fields onto
#' the document without further processing.  It is useful for control arugments
#' like \code{"$limit"} and \code{"$sort"}.
#' 
#' @aliases buildJQuery buildJQterm
#' @param \dots This should be a named list of arguments.  The values should be
#' the desired query value, or a more complex expression (see details).
#' @param rawfields These arguments are passed as character vectors directly
#' into the query document without processing.
#' @param name The name of the field.
#' @param value The value of the field or an expression which gives a query for
#' the resulting document.
#' @return The function \code{buildJQuery} returns a unicode string which
#' contains the JSON query document.  The function \code{buildJQterm} returns a
#' unicode string with just one field in the query document.
#' @author Russell Almond
#' @seealso \code{\link{as.json}}, \code{\link{parseMessage}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#' 
#' 
#' ## Low level test of the JQterm possibilities for fields.
#' 
#' stopifnot(buildJQterm("uid","Fred")=='"uid":"Fred"')
#' stopifnot(buildJQterm("uid",c("Phred","Fred"))=='"uid":{"$in":["Phred","Fred"]}')
#' time1 <- as.POSIXct("2018-08-16 19:12:19 EDT")
#' stopifnot(buildJQterm("time",time1)=='"time":{"$date":1534461139000}')
#' time1l <- as.POSIXlt("2018-08-16 19:12:19 EDT")
#' stopifnot(buildJQterm("time",time1l)=='"time":{"$date":1534461139000}')
#' time2 <- as.POSIXct("2018-08-16 19:13:19 EDT")
#' stopifnot(buildJQterm("time",c(time1,time2))==
#'           '"time":{"$in":[{"$date":1534461139000},{"$date":1534461199000}]}')
#' stopifnot(buildJQterm("time",c(gt=time1))==
#'           '"time":{ "$gt":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(lt=time1))==
#'           '"time":{ "$lt":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(gte=time1))==
#'           '"time":{ "$gte":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(lte=time1))==
#'           '"time":{ "$lte":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(ne=time1))==
#'           '"time":{ "$ne":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(eq=time1))==
#'           '"time":{ "$eq":{"$date":1534461139000} }')
#' stopifnot(buildJQterm("time",c(gt=time1,lt=time2))==
#'           '"time":{ "$gt":{"$date":1534461139000}, "$lt":{"$date":1534461199000} }')
#' stopifnot(buildJQterm("count",c(nin=1,2:4))==
#'           '"count":{"$nin":[1,2,3,4]}')
#' stopifnot(buildJQterm("count",c("in"=1,2:4))==
#'           '"count":{"$in":[1,2,3,4]}')
#' stopifnot(buildJQterm("count",c(ne=1,ne=5))==
#'           '"count":{ "$ne":1, "$ne":5 }')
#' 
#' ## Some Examples of buildJQuery on complete queries.
#' 
#' stopifnot(buildJQuery(app="default",uid="Phred")==
#'           '{ "app":"default", "uid":"Phred" }')
#' stopifnot(buildJQuery("_id"=c(oid="123456789"))==
#'           '{ "_id":{ "$oid":"123456789" } }')
#' stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5))==
#'           '{ "name":"George", "count":{ "$gt":3, "$lt":5 } }')
#' stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5),
#'                       rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))==
#'           '{ "name":"George", "count":{ "$gt":3, "$lt":5 }, "$limit":1, "$sort":{timestamp:-1} }')
#' 
#' 
#' ## Queries on IDs need special handling
#' stopifnot(buildJQuery("_id"=c(oid="123456789abcdef"))==
#'           '{ "_id":{ "$oid":"123456789abcdef" } }')
#' 
#' 
#' 
#' 
#' @export buildJQuery
buildJQuery <- function (...,rawfields=character()) {
  terms <- list(...)
  fields <- names(terms)
  jstrings <- sapply(fields,function(f) buildJQterm(f,terms[[f]]))
  jstrings <- c(jstrings,rawfields)
  query <- paste('{',paste(jstrings,collapse=", "),'}')
  flog.trace("Query = ",query,capture=TRUE)
  query
}


#' Fetches Messages from a Mongo databas
#' 
#' 
#' This function fetches \code{\linkS4class{P4Message}} objects from a
#' \code{\link[mongolite]{mongo}} database.  The message parser is passed as an
#' argument, allowing it to fetch other kinds of objects than P4Messages.  The
#' function \code{getManyRecs} retrieves all matching objects and the function
#' \code{getOneRec} retrieves the first matching object.
#' 
#' 
#' This function assumes that a number of objects (usually, but not necessarily
#' subclasses of \code{\link{P4Message}} objects) have been stored in a Mongo
#' database.  The \code{col} argument is the \code{\link[mongolite]{mongo}}
#' object in which they are stored.  These functions retrive the selected
#' objects.
#' 
#' The first argument should be a string containing a JSON query document.
#' Normally, thes are constructed through a call to \code{\link{buildJQuery}}.
#' 
#' The query is used to create an iterator over JSON documents stored in the
#' database.  At each round, the iterator extracts the JSON document as a
#' (nested) list structure.  This is pased to the \code{parser} function to
#' build an object of the specified type.  See the \code{\link{parseMessage}}
#' function for an example parser.
#' 
#' The sorting argument controls the way the returned list of objects is
#' sorted. This should be a numeric vector with names giving the field for
#' sorting.  The default values \code{c("timestamp"=1)} and
#' \code{c("timestamp"=-1)} sort the records in ascending and decending order
#' respectively.  In particular, the default value for \code{getOneRec} means
#' that the most recent value will be returned.  The defaults assume that
#' \dQuote{timestamp} is a field of the stored object.  To supress sorting of
#' outputs, use \code{NULL} as the argument to \code{sort}.
#' 
#' @aliases getOneRec getManyRecs
#' @param jquery A string providing a Mongo JQuery to select the appropriate
#' records.  See \code{\link{buildJQuery}}.
#' @param col A \code{\link[mongolite]{mongo}} collection object to be queried.
#' @param parser A function which will take the list of fields returned from
#' the database and build an appropriate R object.  See
#' \code{\link{parseMessage}}.
#' @param sort A named numeric vector giving sorting instructions.  The names
#' should correpond to fields of the objects, and the values should be positive
#' or negative one for increasing or decreasing order. Use the value
#' \code{NULL} to leave the results unsorted.
#' @param limit A numeric scalar giving the maximum number of objects to
#' retrieve.  If 0, then all objects matching the query will be retrieved.
#' @return
#' 
#' The function \code{getOneRec} returns an object whose type is determined by
#' the output of the \code{parser} function.  If \code{\link{parseMessage}} is
#' used, this will be a \code{\linkS4class{P4Message}} object.
#' 
#' The function \code{getManyRecs} returns a list of object whose type is
#' determined by the output of the \code{parser} function.
#' @author Russell Almond
#' @seealso \code{\link{saveRec}}, \code{\link{parseMessage}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#' 
#' 
#' \dontrun{
#' ## Requires Mongo test database to be set up.
#' 
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' ## collection = Messages
#' ## Execute in Mongo Shell
#' ## db.createUser({
#' ## ... user: "test",
#' ## ... pwd: "secret",
#' ## ... roles: [{role: "readWrite", db: "test"}]
#' ## ... });
#' 
#' 
#' 
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' 
#' m1@data$time <- list(tim=25.4,units="secs")
#' m1 <- saveRec(m1,testcol)
#' 
#' ## Note use of oid keyword to fetch object by Mongo ID.
#' m1a <- getOneRec(buildJQuery("_id"=c(oid=m1@"_id")),testcol,parseMessage)
#' stopifnot(all.equal(m1,m1a))
#' 
#' m123 <- getManyRecs(buildJQuery(uid="Fred"),testcol,parseMessage)
#' m23 <- getManyRecs(buildJQuery(uid="Fred",sender=c("EI","EA")),
#'                    testcol,parseMessage)
#' m321 <- getManyRecs(buildJQuery(uid="Fred",timestamp=c(lte=Sys.time())),
#'             testcol,parseMessage,sort=c(timestamp=-1))
#' getManyRecs(buildJQuery(uid="Fred",
#'                         timestamp=c(gte=Sys.time()-as.difftime(1,units="hours"))),
#'                         testcol,parseMessage)
#' }
#' 
#' 
#' @export getOneRec
getOneRec <- function(jquery,col,parser,sort=c("timestamp"=-1)) {
  sorts <- paste('{',paste(paste('"',names(sort),'":',sort,sep=""),
                           collapse=", "),'}')
  it <- col$iterate(jquery,'{}',sort=sorts,limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  do.call(parser,list(rec))
}

getManyRecs <- function(jquery,col,parser,sort=c("timestamp"=1),
                        limit = 0) {
  sorts <- paste('{',paste(paste('"',names(sort),'":',sort,sep=""),
                           collapse=", "),'}')

  n <- col$count(jquery)
  if (limit>0) n <- min(n,limit)
  result <- vector("list",n)
  it <- col$iterate(jquery,'{}',sort=sorts,limit=limit)
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- do.call(parser,list(rec))
    nn <- nn +1
  }
  result
}




#' Saves a MongoRec object to a Mongo database
#' 
#' 
#' This function saves an S4 object as a record in a Mongo databalse.  It uses
#' \code{\link{as.json}} to covert the object to a JSON string.
#' 
#' 
#' @param rec The message (object) to be saved.
#' @param col A mongo collection object, produced with a call to
#' \code{\link[mongolite]{mongo}()}. This can also be \code{NULL}, in which
#' case the message will not be saved.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#' 
#' Returns the message argument, which may be modified by setting the
#' \code{"_id"} field if this is the first time saving the object.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{as.json}}, \code{\linkS4class{P4Message}},
#' \code{\link{parseRec}}, \code{\link{getOneRec}},
#' \code{\link[mongolite]{mongo}}
#' @keywords database
#' @examples
#' 
#' 
#' \dontrun{## Need to set up database or code won't run.
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' 
#' ## Save them back to capture the ID.
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' 
#' 
#' 
#' }
#' 
#' @export saveRec
saveRec <- function (rec, col, serialize=TRUE) {
  if (!is.null(col)) {
    jso <- as.json(rec,serialize)
    if (is.na(rec@"_id")) {
      ## Insert
      col$insert(jso)
      it <- col$iterate(jso,'{"_id":true}',limit=1)
      rec@"_id" <- it$one()$"_id"
      names(rec@"_id") <- "oid" ## Aids in extraction
    } else {
      if (col$count(paste('{"_id":{"$oid":"',rec@"_id",'"}}',sep=""))) {
        ## Replace
        col$update(paste('{"_id":{"$oid":"',rec@"_id",'"}}',sep=""),
                   paste('{"$set":',jso,'}',sep=""))
      } else {
        ## ID is out of date, insert and get new ID.
        col$insert(jso)
        it <- col$iterate(jso,'{"_id":true}',limit=1)
        rec@"_id" <- it$one()$"_id"
        names(rec@"_id") <- "oid" ## Aids in extraction
      }
    }
  } else {
    flog.trace("DB is null, not saving recage.")
  }
  rec
}

#' Saves a MongoRec object to a Mongo database
#' 
#' 
#' This function saves an S4 object as a record in a Mongo databalse.  It uses
#' \code{\link{as.json}} to covert the object to a JSON string.
#' 
#' 
#' @param rec The message (object) to be saved.
#' @param col A mongo collection object, produced with a call to
#' \code{\link[mongolite]{mongo}()}. This can also be \code{NULL}, in which
#' case the message will not be saved.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#' 
#' Returns the message argument, which may be modified by setting the
#' \code{"_id"} field if this is the first time saving the object.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{as.json}}, \code{\linkS4class{P4Message}},
#' \code{\link{parseRec}}, \code{\link{getOneRec}},
#' \code{\link[mongolite]{mongo}}
#' @keywords database
#' @examples
#' 
#' 
#' \dontrun{## Need to set up database or code won't run.
#' m1 <- P4Message("Fred","Task1","PP","Task Done",
#'                 details=list("Selection"="B"))
#' m2 <- P4Message("Fred","Task1","EI","New Obs",
#'                 details=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- P4Message("Fred","Task1","EA","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#' 
#' testcol <- mongo("Messages",
#'                  url="mongodb://test:secret@127.0.0.1:27017/test")
#' ## Mongodb is the protocol
#' ## user=test, password =secret
#' ## Host = 127.0.0.1 -- localhost
#' ## Port = 27017 -- Mongo default
#' ## db = test
#' ## collection = Messages
#' 
#' ## Save them back to capture the ID.
#' m1 <- saveRec(m1,testcol)
#' m2 <- saveRec(m2,testcol)
#' m3 <- saveRec(m3,testcol)
#' 
#' 
#' 
#' }
#' 
#' @export saveRec
saveRec <- function (rec, col, serialize=TRUE) {
  if (!is.null(col)) {
    jso <- as.json(rec,serialize)
    if (is.na(rec@"_id")) {
      ## Insert
      col$insert(jso)
      it <- col$iterate(jso,'{"_id":true}',limit=1)
      rec@"_id" <- it$one()$"_id"
      names(rec@"_id") <- "oid" ## Aids in extraction
    } else {
      if (col$count(paste('{"_id":{"$oid":"',rec@"_id",'"}}',sep=""))) {
        ## Replace
        col$update(paste('{"_id":{"$oid":"',rec@"_id",'"}}',sep=""),
                   paste('{"$set":',jso,'}',sep=""))
      } else {
        ## ID is out of date, insert and get new ID.
        col$insert(jso)
        it <- col$iterate(jso,'{"_id":true}',limit=1)
        rec@"_id" <- it$one()$"_id"
        names(rec@"_id") <- "oid" ## Aids in extraction
      }
    }
  } else {
    flog.trace("DB is null, not saving recage.")
  }
  rec
}


###
## This is a construction I find myself using in a lot of places to
## build up the "mongodb://" URI for the database.



#' Creates the URI needed to connect to a mongo database.
#' 
#' 
#' This function formats the universal record indicator (URI) for connecting to
#' a Mongo database.  It is mostly a utility function for formatting the
#' string.
#' 
#' 
#' @param username The name of the database user (login credential), or an
#' empty string if no username is required.
#' @param password The name of the database password (login credential), or an
#' empty string if no password is required.
#' @param host The name or IP address of the system hosting the database.
#' @param port The port to be used for connections.  Note that the port for a
#' default configuration of mongo is 27018.  This can be left blank to use the
#' default port.
#' @param protocol A character scalar giving the protocol to use when
#' connecting, e.g., \dQuote{mongodb}.
#' @return
#' 
#' A character string giving the database URI which can be passed to the
#' \code{\link[mongolite]{mongo}} function to create a database collection
#' handle.
#' 
#' Note that the password is stored in clear text, so appropriate care should
#' be taken with the result of this function.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link{MongoDB}}, \code{\link[mongolite]{mongo}}
#' 
#' This is an input argument to a number of other classes which use mongo
#' connections.
#' @keywords interface database
#' @examples
#' 
#' 
#' stopifnot(makeDBuri()=="mongodb://localhost")
#' 
#' stopifnot(makeDBuri(user="admin",password="secret")==
#'                     "mongodb://admin:secret@localhost")
#' ## No password
#' stopifnot(makeDBuri(user="admin")==
#'                     "mongodb://admin@localhost")
#' 
#' stopifnot(makeDBuri(host="example.com",port=12345) ==
#'           "mongodb://example.com:12345")
#' 
#' 
#' @export makeDBuri
makeDBuri <- function(username="",password="", host="localhost",
                      port="",protocol="mongodb") {
  ## Setup DB URI
  security <- ""
  if (nchar(username) > 0L) {
    if (!is.null(password) && nchar(password) > 0L)
      security <- paste(username,password,sep=":")
    else
      security <- username
  }
  if (nchar(port) > 0L)
    host <- paste(host,port,sep=":")
  else
    host <- host
  if (nchar(security) > 0L)
    host <- paste(security,host,sep="@")
  paste(paste(protocol,":/",sep=""),host,sep="/")
}
