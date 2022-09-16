call_name2 <- function (cal) {
  if (is(cal,"refMethodDef")) {
    name <- attributes(cal)$name
  } else {
    name <- as.character(cal[[1]])
  }
  name[length(name)]
}

call_args2 <- function(fun, call) {
  rlang::call_args(rlang::call_match(fun,cal,defaults=TRUE))
}

fake_mongo <-
  setRefClass("fake_mongo",
              fields=c(collection="character",
                       db="character",
                       url="character",
                       verbose="logical",
                       options="ANY",
                       position="integer",
                       transactions="list",
                       responses="list"),
              methods=list(
                  setTransactions = function(calls, responses) {
                    checkmate::assert_list(calls)
                    checkmate::assert_list(responses)
                    if (length(calls) != length(responses))
                      stop("Calls and responses must be the same length.")
                    transactions <<- transactions
                    responses <<- responses
                    position <<- 0L
                    invisible(position)
                  },
                  testTrans = function () {
                    actTrans <- sys.call(sys.parent())
                    targetFun <- sys.call(sys.parent())
                    position <<- position + 1L
                    expTrans <- transactions[[position]]
                    if (position > length(transactions))
                      testthat::fail("Got more transactions than expected.")
                    if (call_name2(actTrans) ==
                        call_name2(expTrans))
                      testthat::fail(paste("Expected ",
                                           call_name2(expTrans),
                                           "but got",
                                           call_name2(actTrans)))
                    expArgs <- call_args2(targetFun,expTrans)
                    actArgs <- call_args2(targetFun,actTrans)
                    testArgs <- all.equal(expArgs,actArgs)
                    if (!isTRUE(testArgs)) {
                      testthat::fail(paste("In transaction ",call_name2(expTrans),
                                 "argument mismatch:\n",
                                 paste(testArgs,collapse="\n")))
                    }
                    testthat::succeed();
                  },
                  aggregate = function (pipeline='{}', handler=NULL,
                                        pagesize = 1000, iterate=FALSE) {
                    testTrans()
                    responses[[position]]
                  },
                  count = function (query = '{}') {
                    testTrans()
                    responses[[position]]
                  },
                  disconnect = function (gc = TRUE) {
                    testTrans()
                    responses[[position]]
                  },
                  distinct = function (key, query = '{}') {
                    testTrans()
                    responses[[position]]
                  },
                  drop = function () {
                    testTrans()
                    responses[[position]]
                  },
                  export = function (con=stdout(), bson=FALSE,
                                     query = '{}', fiels = '{}',
                                     sort = '{"_id":1}') {
                    testTrans()
                    responses[[position]]
                  },
                  find = function (query = '{}', fields = '{"_id":0}',
                                   sort = '{}', skip = 0,
                                   limit=0, handler = NULL,
                                   pagesize = 1000) {
                    testTrans()
                    responses[[position]]
                  },
                  import = function (con, bson=FALSE) {
                    testTrans()
                    responses[[position]]
                  },
                  index = function (add=NULL, remove=NULL) {
                    testTrans()
                    responses[[position]]
                  },
                  info = function () {
                    testTrans()
                    responses[[position]]
                  },
                  insert = function (data, pagesize= 1000,
                                     stop_on_error = TRUE, ...) {
                    testTrans()
                    responses[[position]]
                  },
                  iterate = function (query = '{}', fields = '{"_id":0}',
                                      sort = '{}', skip = 0,
                                      limit = 0) {
                    testTrans()
                    responses[[position]]
                  },
                  mapreduce = function (map, reduce, query = '{}',
                                        sort = '{}', limit = 0) {
                    testTrans()
                    responses[[position]]
                  },
                  remove = function (query = '{}', just_one = FALSE) {
                    testTrans()
                    responses[[position]]
                  },
                  rename = function (name, db=NULL) {
                    testTrans()
                    responses[[position]]
                  },
                  replace = function (query, update='{}', upsert=FALSE) {
                    testTrans()
                    responses[[position]]
                  },
                  run = function (command = '{"ping":1}', simplify = TRUE) {
                    testTrans()
                    responses[[position]]
                  },
                  update = function (query, update='{"$set":{}}',
                                     upsert = FALSE, multiple = FALSE) {
                    testTrans()
                    responses[[position]]
                  })
              )


fake_mongo <-
function (collection = "test", db = "test", url = "mongodb://localhost",
          verbose = FALSE, options = mongolite::ssl_options()) {
  new ("fake_mongo",collection=collection, db=db, url=url,
       verbose=verbose, options=options, position=0L, transactions=list(),
       responses=list())
}


#' Class \code{"MongoDB"}
#'
#'
#' An S4-style class for the \code{\link[mongolite]{mongo}} class.  Note that
#' this is actually a class union, allowing for \code{NULL} if the database is
#' not yet initialized.
#'
#'
#' @name MongoDB-class
#' @aliases MongoDB-class MongoDB
#' @docType class
#' @note
#'
#' The original \code{\link[mongolite]{mongo}} class is an S3 class.  Rather
#' than just call \code{\link[methods]{setOldClass}} and exposing that, I've
#' explosed a class union (\code{\link[methods]{setClassUnion}}) with the
#' \code{mongo} class and \code{NULL}.
#'
#' A typical usage would have this type used in the slot of an object, which
#' would initialize the value to \code{NULL}, and then set it to a \code{mongo}
#' object when the database connection is openned.
#' @section Objects from the Class:
#'
#' \code{NULL} is an object of this class.
#'
#' Objects of this class can be created with calls to
#' \code{\link[mongolite]{mongo}}.
#' @author Russell Almond
#' @seealso \code{\link{ListenerSet}}, \code{\link[mongolite]{mongo}}
#' @keywords classes
#' @examples
#'
#' showClass("MongoDB")
#' showClass("ListenerSet")
#' lset <- ListenerSet$new()
#' lset$messdb
#'
#setOldClass("mongolite::mongo")
setClassUnion("MongoDB",c("fake_mongo", "NULL","ANY"))
#setIs("mongo","MongoDB")



