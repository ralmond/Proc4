#' Class \code{"MongoRec"}
#'
#'
#' This is a lightweight class meant to be extended.  It contains a
#' single field for a mongo identifier, which can be accessed using
#' the `m_id()` method.  It is meant to store something that is a
#' record in a mongo collection, where `_id` is the mongo identifier.
#'
#'
#'
#' @name MongoRec-class
#' @aliases MongoRec-class m_id,MongoRec-method m_id<-,MongoRec-method
#' as.jlist,MongoRec,list-method as.json,MongoRec-method
#' @docType class
#' @section Objects from the Class:
#'
#' Objects can be created by calls to the \code{\link{MongoRec}()} function.
#' @author Russell G. Almond
#' @seealso \code{\link{MongoRec}()} --- constructor
#' \code{\link{as.json}},
#' \code{\link{parseObject}}, \code{\link{saveRec}}, \code{\link{getOneRec}}
#' @keywords classes
#' @examples
#'
#' showClass("MongoRec")
#'
#' @exportClass("MongoRec")
setClass("MongoRec",
         slots=c("_id"="character"    #Mongo ID
                 ))
#' @describeIn MongoRec
setGeneric("m_id",function(x) standardGeneric("m_id"))

#' @describeIn MongoRec
setGeneric("m_id<-",function(x, value) standardGeneric("m_id<-"))

#' @describeIn MongoRec
setMethod("m_id","MongoRec", function(x) x@"_id")

#' @describeIn MongoRec
setMethod("m_id<-","MongoRec", function(x,value) {
  names(value) <- "oid"
  x@"_id" <- value
  x})

#' @describeIn MongoRec
MongoRec <- function(...,m_id=NA_character_)
  new("MongoRec", "_id"=c(oid=m_id))

#' Converts P4 messages to JSON representation
#'
#'
#' These methods extend the \code{\link[jsonlite]{toJSON}} function providing
#' an extensible protocol for serializing S4 objects.  The function
#' \code{as.json} turns the object into a string containing a JSON document by
#' first calling \code{as.jlist} to convert the object into a list and then
#' calling \code{toJSON} to do the work.
#'
#'
#' The existing \code{\link[jsonlite]{toJSON}} does not support S4 objects, and
#' the \code{\link[jsonlite]{serializeJSON}} provides too much detail; so while
#' it is good for saving and restoring R objects, it is not good for sharing
#' data between programs.  The function \code{as.json} and \code{as.jlist} are
#' S4 generics, so they can be easily extended to other classes.
#'
#' The default method for \code{as.json} is essentially \code{toJSON(
#' as.jlist(x, attributes(x)))}.  The function \code{attributes(x)} turns the
#' fields of the object into a list, and then the appropriate method for
#' \code{as.jlist} further processes those objects.  For example, it can set
#' the \code{"_id"} field used by the Mongo DB as a unique identifier (or other
#' derived fields) to \code{NULL}.
#'
#' Another important step is to call \code{unboxer} on fields which should not
#' be stored as vectors.  The function \code{toJSON} by default wraps all R
#' objects in \sQuote{[]} (after all, they are all vectors), but that is
#' probably not useful if the field is to be used as an index.  Wrapping the
#' field in \code{unboxer()}, i.e., using \code{ml$field <- unboxer(ml$field)},
#' suppresses the brackets.  The function \code{unboxer()} in this package is
#' an extension of the \code{jsonlite::\link[jsonlite]{unbox}} function, which
#' does not properly unbox POSIXt objects.
#'
#' Finally, for a field that can contain arbitrary R objects, the function
#' \code{\link{unparseData}} coverts the data into a JSON string which will
#' completely recover the data.  The \code{serialize} argument is passed to
#' this function.  If true, then \code{\link[jsonlite]{serializeJSON}} is used
#' which produces safe, but not particularly human editable JSON.  If false, a
#' simpler method is employed which produes more human readable code.  This
#' with should work for simpler data types, but does not support objects, and
#' may fail with complex lists.
#'
#' @aliases as.json as.json,ANY-method as.jlist as.jlist,ANY,list-method
#' @param x An (S4) object to be serialized.
#' @param obj The object being serialized
#' @param ml A list of fields of the object; usually \code{attributes(obj)}.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#'
#' The function \code{as.json} returns a unicode string with a serialized
#' version of the object.
#'
#' The function \code{as.jlist} returns a list of the fields of the object
#' which need to be serialized (usually through a call to
#' \code{\link[jsonlite]{toJSON}}.
#' @author Russell Almond
#' @seealso In this package: \code{\link{parseJSON}}, \code{\link{saveRec}},
#' \code{\link{parseData}}, \code{\link{parseData}}
#'
#' In the jsonlite package: \code{\link[jsonlite]{toJSON}},
#' \code{\link[jsonlite]{serializeJSON}},
#' \code{jsonlite::\link[jsonlite]{unbox}}
#' @keywords IO interfaces
#' @examples
#'
#' mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
#'          as.POSIXct("2018-11-04 21:15:25 EST"),
#'          list(correct=TRUE,seletion="D"))
#' as.json(mess1)
#' as.json(mess1,FALSE)
#'
#' \dontrun{
#' ## This is the method for P4 Messages.
#' setMethod("as.jlist",c("P4Message","list"), function(obj,ml) {
#'   ml$"_id" <- NULL
#'   ml$class <-NULL
#'   ## Use manual unboxing for finer control.
#'   ml$app <- unboxer(ml$app)
#'   ml$uid <- unboxer(ml$uid)
#'   if (!is.null(ml$context) && length(ml$context)==1L)
#'     ml$context <- unboxer(ml$context)
#'   if (!is.null(ml$sender) && length(ml$sender)==1L)
#'     ml$sender <- unboxer(ml$sender)
#'   if (!is.null(ml$mess) && length(ml$mess)==1L)
#'     ml$mess <- unboxer(ml$mess)
#'   ml$timestamp <- unboxer(ml$timestamp) # Auto_unboxer bug.
#'   ## Saves name data; need recursvie version.
#'   ml$data <- unparseData(ml$data)
#'   ml
#'   })
#' }
#'
#' @export as.json as.jlist
#' @exportMethod as.json as.jlist
setGeneric("as.json",function(x,serialize=TRUE,
                              dataframe = c("rows", "columns", "values"),
                              matrix = c("rowmajor","columnmajor"),
                              Date = c("ISO8601", "epoch"),
                              POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                              factor = c("string", "list"),
                              complex = c("string", "list"),
                              raw = c("base64", "hex", "mongo", "int", "js"),
                              null = c("list", "null"),
                              na = c("null", "string"))
  standardGeneric("as.json"))

#' @describeIn as.json
setGeneric("as.jlist",function(obj,ml,serialize=TRUE)
  standardGeneric("as.jlist"))

#' @describeIn as.json
setMethod("as.json","ANY",
          function(x,serialize=TRUE,
                   dataframe = c("rows", "columns", "values"),
                   matrix = c("rowmajor","columnmajor"),
                   Date = c("ISO8601", "epoch"),
                   POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                   factor = c("string", "list"),
                   complex = c("string", "list"),
                   raw = c("base64", "hex", "mongo", "int", "js"),
                   null = c("list", "null"),
                   na = c("null", "string")) {
    jlist <- as.jlist(x,attributes(x),serialize)
    toJSON(jlist, dataframe[1], matrix[1], Date[1], POSIXt[1],
           factor[1], complex[1],
           raw[1], null[1], na[1])
})

#' @describeIn as.json
setMethod("as.json","MongoRec",
          function(x,serialize=TRUE,
                   dataframe = c("rows", "columns", "values"),
                   matrix = c("rowmajor","columnmajor"),
                   Date = c("ISO8601", "epoch"),
                   POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                   factor = c("string", "list"),
                   complex = c("string", "list"),
                   raw = c("base64", "hex", "mongo", "int", "js"),
                   null = c("list", "null"),
                   na = c("null", "string")) {
            ## Different defaults for Mongo
            if (missing(POSIXt)) POSIXt <- "mongo"
            if (missing(raw)) raw <- "mongo"
            jlist <- as.jlist(x,attributes(x),serialize)
            toJSON(jlist, dataframe[1], matrix[1],
                   Date[1], POSIXt[1], factor[1], complex[1],
                   raw[1], null[1], na[1])
          })

#' @describeIn as.json
setMethod("as.jlist",c("ANY","list"), function(obj,ml,serialize=TRUE) {
  ml
})

#' @describeIn MongoRec
setMethod("as.jlist",c("MongoRec","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  callNextMethod(obj,ml,serialize)
})

#' @describeIn parseObject
setGeneric("parse.jlist",function(class,rec)
  standardGeneric("parse.jlist"))


#' @describeIn parseObject
## Base case for call-next-method.
setMethod("parse.jlist",c("ANY","list"),
          function(class,rec) {
            rec
            })


#' Construct an S4 object from a list of its slot values.
#'
#' This function takes the list produced from parsing a raw json object using
#' \code{\link[jsonlite]fromJSON}}, processes is using the function \code{parse.jlist}
#' to massage the elements, and then passes it to the \code{new} function
#' to create a new object of type \code{class}.
#'
#' The \code{parse.jlist} function is a helper function designed to do any massaging
#' necessary to unencode the slot values before the object is produced.  The function
#' \code{\link{ununboxer}} undoes the effect of \code{unboxer}, and the
#' function \code{\link{unparseData}} udoes the effect of \code{parseData}.
#'
#' @param rec -- A list which is the output of \code{\link[jsonlite]{fromJSON}}
#' @param class  -- A character string defining the class of the output object.
#'     If the list has an element named `class`, that will be used.
#'
#' @return An S4 object of type `class`
#' @export
#'
#' @examples
parseObject <- function (rec, class=rec$class) {
  if (is.list(class) && length(class)==1L)
    ## toJSON has wrapped the class name, fix.
    class <- as.character(class[[1]])
  ## Maybe I shouldn't have named the argument class.
  if (isS4(class)) class <- class(class)
  class <- getClass(class)
  rec <- as.list(rec)
  jlp <- selectMethod("parse.jlist",c(class,"list"))
  if (!is.null(jlp))
    rec <- do.call(jlp,list(class,rec))
  rec$class <- NULL # Make sure it is not marked as an extra argument.
  do.call("new",c(class,rec))
}

#' @describeIn MongoRec
setMethod("parse.jlist",c("MongoRec","list"), function(class, rec) {
  if (is.null(rec$"_id"))
    id <- NA_character_
  else
    id <- as.character(ununboxer(rec$"_id"))
  if (is.null(names(id)))  names(id) <- "oid"
  rec$"_id" <- id
  callNextMethod(class, rec)
})



#' Marks scalar objects to be preserved when converting to JSON
#'
#'
#' The function \code{\link[jsonlite]{toJSON}} coverts vectors (which all R
#' objects are) to vectors in the JSON code.  The function
#' \code{jsonlite::\link[jsonlite]{unbox}} protects the object from this
#' behavior, which makes the fields eaiser to search and protects against loss
#' of name attributes.  The function \code{unboxer} extents \code{unbox} to
#' recursively unbox lists (which preserves names).  The function
#' \code{ununbox} removes the unboxing flag and is mainly used for testing
#' parser code.
#'
#'
#' The \code{jsonlite::\link[jsonlite]{unbox}} function does not necessarily
#' preserve the name attributes of elements of the list.  In other words the
#' sequence \code{\link{as.jlist}} -> \code{\link[jsonlite]{toJSON}} ->
#' \code{\link[jsonlite]{fromJSON}} -> \code{\link{parseMessage}} might not be
#' the identity.
#'
#' The solution is to recursively apply \code{\link[jsonlite]{unbox}} to the
#' elements of the list.  The function \code{unboxer} can be thought of as a
#' recursive version of \code{unbox} which handles the entire tree struction.
#' If \code{x} is not a list, then \code{unboxer} and \code{unbox} are
#' equivalent.
#'
#' The typical use of this function is defining methods for the
#' \code{\link{as.jlist}} function.  This gives the implementer fine control of
#' which attributes of a class should be scalars and vectors.
#'
#' The function \code{ununbox} clears the unboxing flag.  Its main purpose is
#' to be able to test various parsers.
#'
#' @aliases unboxer ununboxer
#' @param x Object to be boxed/unboxed.
#' @return The function \code{unboxer} returns the object with the added class
#' \code{scalar}, which is the \code{jsonlite} marker for a scalar.
#'
#' The function \code{ununboxer} returns the object without the \code{scalar}
#' class marker.
#' @note
#'
#' There is a bug in the way that \code{\link[base]{POSIXt}} classes are
#' handled, \code{unboxer} fixes that problem.
#' @section Warning: Dependence on jsonlite implementation:
#'
#' These functions currently rely on some internal mechanisms of the jsonline
#' pacakge.  In particular, \code{ununbox} relies on the
#' \dQuote{scalar} class mechanism.
#' @author Russell Almond
#' @seealso \code{\link[jsonlite]{unbox}}, \code{\link[jsonlite]{toJSON}},
#' \code{\link{as.jlist}}, \code{\link{parseMessage}}
#' @keywords interface
#' @examples
#'
#'
#' ## as.jlist method shows typical use of unboxer.
#' getMethod("as.jlist",c("P4Message","list"))
#'
#' ## Use ununboxer to test as.jlist/parseMessage pair.
#' m4 <- P4Message("Phred","Task1","PP","New Stats",
#'                 details=list("agents"=c("ramp","ramp","lever")))
#' m4jl <- as.jlist(m4,attributes(m4))
#' m4a <- parseMessage(ununboxer(m4jl))
#' stopifnot(all.equal(m4,m4a))
#'
#'
#'
#' @export unboxer ununboxer
unboxer <- function (x) {
  if (is(x,"list")) {
    lapply(x,unboxer) #Saves name data.
  } else {
    if (length(x) == 1L) {
      jsonlite::unbox(x)
    } else {
      x
    }
  }
}

## Need this for testing.
#' @describeIn unboxer
ununboxer <- function (x) {
  if (is(x,"scalar"))
    class(x) <- setdiff(class(x),"scalar")
  if (is.list(x))
    x <- lapply(x, function(s) {
      if (is(s,"POSIXt")) {
        ununboxer(s)
      } else {
        sapply(s,ununboxer)
      }})
  x
}



## Older and simpler parser, but this might work with non-serialized
## content.
parseSimpleData <- function (messData) {
  ##Need to convert back from list to numeric/character
  if (length(messData) == 0L) return(list())
  for (i in 1:length(messData)) {
    datum <- messData[[i]]
    if (all(sapply(datum,is.character)) && all(sapply(datum,length)==1L)) {
      datum <- as.character(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.logical)) && all(sapply(datum,length)==1L)) {
      datum <- as.logical(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.numeric)) && all(sapply(datum,length)==1L)) {
      if (all(sapply(datum,is.integer))) {
        datum <- as.integer(datum)
      } else {
        datum <- as.numeric(datum)
      }
      names(datum) <- names(messData[[i]])
    }
    ## May need an extra step here to decode data which
    ## are not one of the primative vector types.
    messData[[i]] <- datum
  }
  messData
}

parseData <- function (messData) {
  if (is.character(messData)) {
    unserializeJSON(messData)
  } else {
    parseSimpleData(messData)
  }
}

unparseData <- function (data,serialize=TRUE) {
  if (serialize)
    unbox(serializeJSON(data))
  else
    unboxer(data)
}


