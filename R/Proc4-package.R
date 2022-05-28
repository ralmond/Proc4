

#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_title("Proc4")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_description("Proc4")}
#' 
#' 
#' The DESCRIPTION file:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_DESCRIPTION("Proc4")}
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_indices("Proc4")}
#' 
#' This package exists to supply core functionality to other processes
#' implementing processes in the four process architecture (Almond, Steinberg
#' and Mislevy, 2002).  In particular, it contains low level code dealing with
#' implementing message queues in a document database (\link[mongolite]{mongo})
#' and reading/writing messages from JSON.
#' 
#' There are five major features of this package documented below: \enumerate{
#' \itemThe \code{\linkS4class{P4Message}} object and the protocol for
#' converting messages to JSON and saving them in the mongo database.  \itemA
#' \code{\link{withFlogging}} function which wraps the
#' \code{\link[futile.logger]{flog.logger}} protocol.  \itemA number of
#' \code{\link{Listener}} objects which implement an observer protocol for
#' messages.  \itemThe \code{config} directory contains a number of javascript
#' files for building database schemas and indexes.  \itemThe \code{dongle}
#' directory contains a number of PHP scripts for exposing the database via a
#' web server. }
#' 
#' @name Proc4-package
#' @aliases Proc4-package Proc4
#' @docType package
#' @section P4 Messages:
#' 
#' The extended four process architecture defines a message object
#' (\code{\linkS4class{P4Message}}) with the following fields:
#' 
#' \describe{ \item{list("_id")}{Used for internal database ID.}\item{:}{Used
#' for internal database ID.} \item{list("app")}{Object of class
#' \code{"character"} which specifies the application in which the messages
#' exit. }\item{:}{Object of class \code{"character"} which specifies the
#' application in which the messages exit. } \item{list("uid")}{Object of class
#' \code{"character"} which identifies the user (student). }\item{:}{Object of
#' class \code{"character"} which identifies the user (student). }
#' \item{list("context")}{Object of class \code{"character"} which identifies
#' the context, task, or item. }\item{:}{Object of class \code{"character"}
#' which identifies the context, task, or item. } \item{list("sender")}{Object
#' of class \code{"character"} which identifies the sender.  This is usually
#' one of "Presentation Process", "Evidence Identification Process", "Evidence
#' Accumulation Process", or "Activity Selection Process".}\item{:}{Object of
#' class \code{"character"} which identifies the sender.  This is usually one
#' of "Presentation Process", "Evidence Identification Process", "Evidence
#' Accumulation Process", or "Activity Selection Process".}
#' \item{list("mess")}{Object of class \code{"character"} a general title for
#' the message context.}\item{:}{Object of class \code{"character"} a general
#' title for the message context.} \item{list("timestamp")}{Object of class
#' \code{"POSIXt"} which gives the time at which the message was
#' generated.}\item{:}{Object of class \code{"POSIXt"} which gives the time at
#' which the message was generated.} \item{list("data")}{Object of class
#' \code{"list"} which contains the data to be transmitted with the
#' message.}\item{:}{Object of class \code{"list"} which contains the data to
#' be transmitted with the message.} \item{list("processed")}{A logical value:
#' true if the message has been processed, and false if the message is still in
#' queue to be processed.  This field is set with
#' \code{\link{markAsProcessed}}.}\item{:}{A logical value: true if the message
#' has been processed, and false if the message is still in queue to be
#' processed.  This field is set with \code{\link{markAsProcessed}}.}
#' \item{list("pError")}{If a error occurs while processing this event,
#' information about the error can be stored here, either as an R object, or as
#' an R object of class error (or any class).  This field is accessed with
#' \code{\link{processingError}} and set with \code{\link{markAsError}}.
#' }\item{:}{If a error occurs while processing this event, information about
#' the error can be stored here, either as an R object, or as an R object of
#' class error (or any class).  This field is accessed with
#' \code{\link{processingError}} and set with \code{\link{markAsError}}. } }
#' 
#' Other classes can extend this message protocol by adding additional fields,
#' but the header fields of the message object allow it to be routed.
#' 
#' In particular, the \code{\link{processed}} field allows a database
#' collection of messages to be used as queue.  Simply search for unprocessed
#' message and begin processing them oldest first, using
#' \code{\link{markAsProcessed}} to mark the complete process and
#' \code{\link{markAsError}} to mark errors.
#' 
#' The functions \code{\link{saveRec}}, \code{\link{getOneRec}} and
#' \code{\link{getManyRecs}} facilitate saving and loading message objects from
#' the database.  These build on the \code{mongolite (\link[mongolite]{mongo})}
#' and \code{jsonlite (\link[jsonlite]{toJSON})} packages.  The function
#' \code{buildJQuery} gives R-like syntactic sugar to building mongo (JSON)
#' queries.
#' 
#' The \code{jsonlite} package provides minimal support for storing S4 objects
#' in the mongo database.  In particular, \code{\link[jsonlite]{toJSON}}
#' provides too little support and \code{\link[jsonlite]{serializeJSON}} wraps
#' the object in R-specific metadata which makes the data difficult for other
#' applications to extract. Instead, \code{Proc4} introduces a new protocol
#' which is suitable for saving S4 classes: a generic \code{\link{as.json}}
#' function for converting the class to JSON, and a \code{parse}\emph{XXX}
#' function for reversing the process.
#' 
#' The \code{\link{as.json}} function calls \code{\link[base]{attributes}} to
#' convert the S4 object into a list, and then calls the function
#' \code{\link{as.jlist}} to massage the elements of the list for export into
#' JSON.  The function \code{\link{unboxer}} is useful for preventing elements
#' which should be scalars from being converted into lists.  The \code{as.json}
#' function then runs the result through \code{\link[jsonlite]{toJSON}} to get
#' the result.
#' 
#' The \code{parse}\emph{XXX} messages reverse this process.  In particular,
#' the record is retrieved from the database and converted into a list using
#' \code{\link[jsonlite]{fromJSON}}.  The parsing function is then called on
#' the result to build the object.  The function \code{\link{parseMessage}}
#' provides an example.  The function \code{\link{cleanMessageJlist}} does much
#' of the interior work of the parsing and is intended for subclasses of
#' \code{\linkS4class{P4Message}}.  The \code{\link{getOneRec}} and
#' \code{\link{getManyRecs}} functions take a \code{parse}\emph{XXX} function
#' as an argument to construct objects from the database.
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_author("Proc4")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "Proc4")\Sexpr{tools:::Rd_package_maintainer("Proc4")}
#' @seealso \code{\link[futile.logger]{flog.logger}},
#' \code{\link[EIEvent:EIEvent-package]{EIEvent}},
#' \code{\link[EABN:EABN-package]{EABN}}
#' @references
#' 
#' Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing the
#' design and delivery of Assessment Systems: A Four-Process Architecture.
#' \emph{Journal of Technology, Learning, and Assessment}, \bold{1},
#' \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}.
#' @keywords package database
NULL





NULL









