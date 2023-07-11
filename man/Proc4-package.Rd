\name{Proc4-package}
\alias{Proc4-package}
\alias{Proc4}
\docType{package}
\title{
\packageTitle{Proc4}
}
\description{
\packageDescription{Proc4}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Proc4}
\packageIndices{Proc4}

This package exists to supply core functionality to other processes
implementing processes in the four process architecture (Almond,
Steinberg and Mislevy, 2002).  In particular, it contains low level code
dealing with implementing message queues in a document database
(\link[mongolite]{mongo}) and reading/writing messages from JSON.

There are five major features of this package documented below:
\enumerate{
  \item{The \code{\linkS4class{P4Message}} object and the protocol for
    converting messages to JSON and saving them in the mongo database.}
  \item{A \code{\link{withFlogging}} function which wraps the
    \code{\link[futile.logger]{flog.logger}} protocol.}
  \item{A number of \code{\link{Listener}} objects which implement an
    observer protocol for messages.}
  \item{The \code{config} directory contains a number of javascript
    files for building database schemas and indexes.}
  \item{The \code{dongle} directory contains a number of PHP scripts for
    exposing the database via a web server.}
}

Earlier verisons included a number of tools which wrap funcions in the
\code{\link[mongolite:mongo]{mongolite}} and
\code{\link[jsonlite:fromJSON]{jsonlite}} packages.  In particular, this
included the \code{\link[mongo]{as.json}},
\code{\link[mongo]{parse.json}}, and \code{\link[mongo]{buildObject}}
generic functions to manage the conversion from S4 object to JSON and
back, the \code{\link[mongo]{saveRec}},
\code{\link[mongo]{getOneRec}}, and \code{\link[mongo]{getManyRecs}}
methods for saving and restoring objects from a database, and the
\code{\link[mongo]{buildJQuery}} for building Mongo queries from R-like
syntax.  These have been moved to the
\code{\link[mongo:mongo-package]{mongo}} package.

}
\section{P4 Messages}{

  The extended four process architecture defines a message object
  (\code{\linkS4class{P4Message}}) with the following fields:

  \describe{
    \item{\code{_id}:}{Used for internal database ID.}
    \item{\code{app}:}{Object of class \code{"character"} which
      specifies the application in which the messages exit. }
    \item{\code{uid}:}{Object of class \code{"character"} which
      identifies the user (student). }
    \item{\code{context}:}{Object of class \code{"character"} which
      identifies the context, task, or item. }
    \item{\code{sender}:}{Object of class \code{"character"} which
      identifies the sender.  This is usually one of
      "Presentation Process", "Evidence Identification Process",
      "Evidence Accumulation Process", or "Activity Selection Process".}
    \item{\code{mess}:}{Object of class \code{"character"} a general
      title for the message context.}
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} which gives
      the time at which the message was generated.}
    \item{\code{data}:}{Object of class \code{"list"} which contains the
      data to be transmitted with the message.}
    \item{\code{processed}:}{A logical value:  true if the message has
      been processed, and false if the message is still in queue to be
      processed.  This field is set with \code{\link{markAsProcessed}}.}
    \item{\code{pError}:}{If a error occurs while processing this event,
      information about the error can be stored here, either as an R
      object, or as an R object of class error (or any class).  This
      field is accessed with \code{\link{processingError}} and set with
      \code{\link{markAsError}}. }
  }

  Other classes can extend this message protocol by adding additional
  fields, but the header fields of the message object allow it to be
  routed.

  In particular, the \code{\link{processed}} field allows a database
  collection of messages to be used as queue.  Simply search for
  unprocessed message and begin processing them oldest first, using
  \code{\link{markAsProcessed}} to mark the complete process and
  \code{\link{markAsError}} to mark errors.

  The \code{mongo::\link[mongo]{as.json}} and
  \code{mongo::\link[mongo]{parse.json}} functions build JSON
  representations of S classes.  In general, this process needs explicit
  instructions on how to code/decode the fields of the object.  Methods
  of the inner \code{mongo::\link[mongo]{as.jlist}} and
  \code{mongo::\link[mongo]{parse.jlist}} provide this functionality.
  Note that classes which extend \code{\linkS4class{P4Message}} class
  will need to use these methods.  The \code{\link{cleanMessageJlist}}
  does the common processing for the \code{P4Message} parent class.
  Finally, \code{\link{buildMessage}} is a more specific version of the
  \code{\link[mongo]{buildObject}} generic builder.

  The functions \code{\link[mongo]{saveRec}},
  \code{\link[mongo]{getOneRec}} and 
  \code{\link[mongo]{getManyRecs}} facilitate saving and loading message
  objects from the database.  The function \code{\link[mongo]{buildJQuery}}
  gives R-like syntactic sugar to building mongo (JSON) queries.
  
}
\section{Logging}{

  The logging system for the \code{Proc4} processes is mostly just the
  \code{\link[futile.logger]{flog.logger}} protocol.  Aside from
  importing the \code{futile.logger} package, \code{Proc4} adds the
  function \code{\link{withFlogging}} executes a series of statements in
  an environment in which the error messages will be logged, and at
  higher logging levels, stack traces for errors and warnings are given.
  The intention is that most message handling functions will be wrapped
  in \code{\link{withFlogging}}, so that information about the message
  causing the error/warning will be available for debugging.

  The package also supplies a \code{\link{mongoAppender}} class, which
  provides a way of logging messages to a database.
  
}
\section{Listeners}{

  The \code{Proc4} package implements an observer protocol called
  \code{\link{Listener}}.  A listener is an abstract class which
  implements the \code{\link{receiveMessage}} function.  The argument of
  this function is a \code{\linkS4class{P4Message}} object, which the
  listener then does something with.  (In most of the implemented
  examples, this is to save it in a database.)  Note that listeners
  should also define a \code{\link{isListener}} method to indicate that
  it is a listener.

  Four listeners are currently implemented (see
  \code{\link{Listener}} or the individal listener classes):
    \describe{
    \item{\code{CaptureListener}}{Creates an object of class
      \code{\linkS4class{CaptureListener}} which stores the messages in
      a list.}
    \item{\code{InjectionListener}}{Creates an object of class
      \code{\linkS4class{InjectionListener}} which inserts the message into
      the designated database.} 
    \item{\code{UpdateListener}}{Creates an object of class
      \code{\linkS4class{UpdateListener}} which updates the designated
      field.}
    \item{\code{UpsertListener}}{Creates an object of class
      \code{\linkS4class{UpsertListener}} which insert or replaces the
      message in the designated collection.} 
    \item{\code{TableListener}}{Creates an object of class
      \code{\linkS4class{TableListener}} which adds details from message
      to rows of a data frame.}
  }

  The \code{\linkS4class{RefListener}} is an abstract class which
  provides methods for the other classes (in particular, promoting the
  class-based methods to true S4 methods.  These include
  \code{isListener()}, \code{listenerName}, \code{listeningFor},
  \code{receiveMessage}, and \code{clearMessage}.  Note that the default
  methods for the latter two 
  functions rely on internal \code{$receiveMessage()} and
  \code{$reset()} class-based methods, which must be implemented in the
  subclasses.  
  
  The \code{\link{ListenerSet}} class is a mixin to associate a
  collection of listeners with an object (the
  \code{\link[EIEvent]{EIEngine}} and \code{\link[EABN]{BNEngine}}
  classes use this).  The generic function \code{\link{notifyListeners}}
  can be called.  This logs information about the message (see logging
  system above), save a copy of the message in a \dQuote{Messages}
  database, and calls the \code{\link{receiveMessage}} method on all of
  the listener objects in its collection.

}
\section{Configuration Files}{

  Using the mongo database, both security (user IDs and passwords) is
  optional.  Running mongo without security turned on
  is probably okay as long as the installation is (a) behing a firewall,
  and (b) the firewall is configured to not allow connections on the
  mongo port except from localhost.  However, other users may want to
  turn on security.

  The recommended security setup is to create four users, \dQuote{EIP},
  \dQuote{EAP}, \dQuote{ASP}, and \dQuote{C4} for the four processes and
  to assign a password to each.  The URI's of the database connections
  then need to be modified to include the username and passwords.  Each
  process would have an \code{ini.R} file which contains its password
  which is stored in an appropriate configuration directory.  (On *nix
  systems, the recommend location is \code{/usr/local/share/Proc4}.)
  
  The files \code{Proc4.ini} (PHP format) and \code{Proc4.js}
  (javascript format) can be used for saving the key usernames and
  passwords.  These files are located in the directory \code{file.path(
  library(help="Proc4")$path, "config")}.  To install these files it is
  necessary to copy the files to the configuration directory and edit
  them so that the password reflects local preferences.

  The file \code{setupDatabases.js} in the \code{config} directory
  creates databases for each of the processes and stores the appropriate
  login credentials.  (Note that this calls \code{Proc4.js} to get these
  credentials so that file must be established first.)  This is a
  javascript file designed to be run directly in mongo, i.e.,
  \code{mongo setupDatabases.js}.  Note that it must be run by a user
  which has the appropriate priveleges to create databases and modify
  their security (a \dQuote{root} user).

  The file \code{setupProc4.js} in the \code{config} directory sets up
  schemas and indexes for collections in the \code{Proc4} database which
  are used by the dongle process.  Schemas are optional in mongo, but
  the indexes should speed up operations.

}
\section{Dongle Files}{

The directory \code{file.path(library(help="Proc4")$path, "config")}
contains files that facilitate direct communciation with the mongo
database.  In particular, there are a number of PHP scripts which if put
in a directory available to the web server will allow remote processes
to get information about users in the system.  The scripts are:

\describe{
  \item{\code{PlayerStart.php}}{Called when player logs in on a given
    day.  As data returns information needed to restore gaming session
    (currently bank balance and list of trophies earned).  Note that
    player details are updated by the EI process.}
  \item{\code{PlayerStop.php}}{Called when player logs out.  Currently
    not used.  It is designed to help automatically shut down unneeded
    processed.}
  \item{\code{PlayerStats.php}}{Called when current player competency
    estimates are required, e.g., when displaying player scores.  It
    returns a list of statistics and their values in the data field;
    the exact statistics returned depend on the configuration of the
    EA process.  This database collection is updated by the EA process
    after each game level is processed.}
  \item{\code{PlayerLevels.php}}{Called when the game wants the next
    level.  The message data should contain information about what
    topic the player is currently addressing and a list of played and
    unplayed levels, with the unplayed levels sorted so the next level
    according to protocol is first on the list.  The complete list of
    levels should be returned so that if levels on the list have
    already been completed, a new level would be entered.  Although
    the PHP script has been built, the AS process to feed it has not.}
}

In addition, there is a file called \code{LLtoP4} in that directory
which is a \code{bash} script for translating between xAPI and Proc4
message formats.  The function \code{LLtoP4Loop} repeatedly downloads
xAPI statements from the learning locker database, translates them to P4
format, and uploads them to the EI process database.

The vingette file \code{Dongle.pdf} describes the dongle and database
structure in more detail.

}
\author{
\packageAuthor{Proc4}

Maintainer: \packageMaintainer{Proc4}
}
\references{

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}.

  The source code, and issues database can be found at
  \url{https://github.com/ralmond/Proc4}

}
\keyword{ package }
\keyword{ database }
\seealso{
  \code{\link[futile.logger]{flog.logger}},
  \code{\link[EIEvent:EIEvent-package]{EIEvent}},
  \code{\link[EABN:EABN-package]{EABN}} 
}
\section{Acknowledgements}{

  Work on the Proc4, EIEvent and EABN packages has been supported by the
  following grants:
  \itemize{
     \item{Bill and Melinda Gates Foundation grant
       \dQuote{Games as Learning/Assessment:  Stealth Assessment}
       (no. 0PP1035331,  Val Shute, PI)}
     \item{National Science Foundation grant \dQuote{DIP:
	 Game-based Assessment and Support of STEM-related Competencies}
       (no. 1628937, Val Shute, PI)}
     \item{National Scient Foundation grant \dQuote{Mathematical
	 Learning via Architectual Design and Modeling Using E-Rebuild.}
       (no. 1720533, Fengfeng Ke, PI)}
     \item{Institute of Educational Statistics Grant: \dQuote{Exploring
	 adaptive cognitive and affective learning support for
	 next-generation STEM learning games.} (no. R305A170376-20, Val
       Shute and Russell Almond, PIs')}
   }

  The Proc4 package developement was led by Russell Almond (Co-PI).
}
