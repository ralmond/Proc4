## Code taken from
## https://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r

withJavaLogging <- function(expr, silentSuccess=FALSE, stopIsFatal=TRUE) {
    hasFailed <- FALSE
    messages <- list()
    warnings <- list()
    logger <- function(obj) {
        # Change behaviour based on type of message
      level <- sapply(class(obj), switch,
                      debug="DEBUG",
                      message="INFO",
                      warning="WARN",
                      caughtError = "ERROR",
                      error=if (stopIsFatal) "FATAL" else "ERROR", "")
      level <- c(level[level != ""], "ERROR")[1]
      simpleMessage <- switch(level, DEBUG=,INFO=TRUE, FALSE)
      quashable <- switch(level, DEBUG=,INFO=,WARN=TRUE, FALSE)

      ## Format message
      time  <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
      txt   <- conditionMessage(obj)
      if (!simpleMessage) txt <- paste(txt, "\n", sep="")
      msg <- paste(time, level, txt, sep=" ")
      calls <- sys.calls()
      calls <- calls[1:length(calls)-1]
      trace <- limitedLabels(c(calls, attr(obj, "calls")))
      if (!simpleMessage && length(trace) > 0) {
        trace <- trace[length(trace):1]
        msg <- paste(msg, "  ", paste("at", trace, collapse="\n  "), "\n", sep="")
      }

      ## Output message
      if (silentSuccess && !hasFailed && quashable) {
        messages <<- append(messages, msg)
        if (level == "WARN") warnings <<- append(warnings, msg)
      } else {
        if (silentSuccess && !hasFailed) {
          cat(paste(messages, collapse=""))
          hasFailed <<- TRUE
        }
        cat(msg)
      }

      ## Muffle any redundant output of the same message
      optionalRestart = function(r) {
        res <- findRestart(r)
        if (!is.null(res)) invokeRestart(res)
      }
      optionalRestart("muffleMessage")
      optionalRestart("muffleWarning")
    }
    vexpr = withCallingHandlers(withVisible(expr),
                                debug=logger, message=logger, warning=logger,
                                caughtError=logger, error=logger)
    if (silentSuccess && !hasFailed) {
      cat(paste(warnings, collapse=""))
    }
    if (vexpr$visible) vexpr$value else invisible(vexpr$value)
}



#' Invoke expression with errors logged and traced
#' 
#' 
#' This is a version of \code{\link[base]{try}} with a couple of important
#' differences.  First, error messages are redirected to the log, using the
#' \code{\link[futile.logger]{flog.logger}} mechanisms.  Second, extra context
#' information can be provided to aid with debugging.  Third, stack traces are
#' added to the logs to assist with later debugging.
#' 
#' 
#' The various processes of the four process assessment design are meant to run
#' as servers.  So when errors occur, it is important that they get logged with
#' sufficient detail that they can be reproduced, fixed and added to the test
#' suite to prevent recurrance.
#' 
#' First, signals are caught and redirected to the appropriate
#' \code{\link[futile.logger]{flog.logger}} handler.  This has several
#' important advantages.  First, the output can be directed to various files
#' depending on the origin package.  In general, the name of the package should
#' be the name of the logger.  So,
#' \code{flog.appender(appender.file("/var/log/Proc4/EIEvent_log.json"),
#' name="EIEvent")} would log error from the EIEvent package to the named file.
#' Furthermore, \code{flog.layout(layout.json,name="EIEvent")} will cause the
#' log to be in JSON format.
#' 
#' Second, additional context information is logged at the \dQuote{DEBUG} level
#' when an condition is signaled.  The \code{context} string is printed along
#' with the error or warning message.  This can be used, for example, to
#' provide information about the user and task that was being processed when
#' the condition was signaled.  In addition, any of the \code{...} arguments
#' are printed.  This can be used to print information about the message being
#' processed and the initial state of the system, so that the error condition
#' can be reproduced.
#' 
#' Third, if the class of the exception is in the \code{tracelevel} list, then
#' a stack trace will be logged (at the \dQuote{DEBUG} level) along with the
#' error.  This should aid debugging.
#' 
#' Fourth, in the case of an error or fatal error, an object of class
#' \code{try-error} (see \code{\link[base]{try}}).  Among other things, this
#' guarentees that \code{withFlogging} will always return control to the next
#' statement.
#' 
#' @param expr The expression which will be exectued.
#' @param \dots Additional context arguments.  Each additional argument should
#' have an explicit name.  In the case of an error or warning, the additional
#' context details will be added to the log.
#' @param context A string identifying the context in which the error occurred.
#' For example, it can identify the case which is being processed.
#' @param loggername This is passed as the \code{name} argument to
#' \code{\link[futile.logger]{flog.logger}}.  It defaults to the package in
#' which the call to \code{withFlogging} was made.
#' @param tracelevel A character vector giving the levels of conditions for
#' which stack traces should be added to the log.  Should be strings with
#' values \dQuote{TRACE}, \dQuote{DEBUG}, \dQuote{INFO}, \dQuote{WARN},
#' \dQuote{ERROR} or \dQuote{FATAL}.
#' @return
#' 
#' If \code{expr} executes successfully (with no errors or fatal errors) then
#' the value of \code{expr} will be returned.  If an error occurs during
#' execution, then an object of class \code{try-error} will be returned.
#' @author Russell Almond
#' @seealso
#' 
#' \code{\link[base]{try}}, \code{\link[futile.logger]{flog.logger}},
#' \code{\link[futile.logger]{flog.layout}},
#' \code{\link[futile.logger]{flog.appender}}
#' @references
#' 
#' The code for executing the stack trace was taken from
#' \url{https://stackoverflow.com/questions/1975110/printing-stack-trace-and-continuing-after-error-occurs-in-r}
#' @keywords debugging error
#' @examples
#' 
#' 
#' \dontrun{
#' ## Setup to log to file in json format.
#' flog.appender(appender.file("/var/log/Proc4/Proc4_log.json"),
#'     name="Proc4")
#' flog.layout(layout.json,name="EIEvent")
#' }
#' 
#' xy <- withFlogging(stop("shoes untied"),context="walking",foot="left")
#' stopifnot(is(xy,"try-error"))
#' 
#' 
#' xx <- withFlogging(log(-1))
#' stopifnot(is.nan(xx))
#' 
#' withFlogging(log(-1),tracelevel=c("ERROR","FATAL"))
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @export withFlogging
withFlogging <- function(expr,...,context=deparse(substitute(expr)),
                         loggername=flog.namespace(),
                         tracelevel=c("WARN","ERROR","FATAL")) {
  fargs <- list(...)
  tracelevel <- toupper(tracelevel)
  handler <- function(obj) {
    ## Change behaviour based on type of message
    level <- sapply(class(obj), switch,
                    trace="TRACE",
                    debug="DEBUG",
                    message="INFO",
                    warning="WARN",
                    caughtError = "ERROR",
                    error="FATAL", "")
    ## Fixes multiple classes on message.
    level <- c(level[level != ""], "ERROR")[1]
    simpleMessage <- switch(level, DEBUG=,INFO=TRUE, FALSE)

    ## Format message
    txt   <- conditionMessage(obj)
    if (!simpleMessage) txt <- paste(txt, "\n", sep="")
    msg <- paste("While ", context, ", ", level,
                 ifelse(level=="FATAL"," ERROR:  ",":  "),txt, sep="")
    logger <- switch(level,
                     TRACE=flog.trace,
                     DEBUG=flog.debug,
                     INFO=flog.info,
                     WARN=flog.warn,
                     ERROR=flog.error,
                     FATAL=flog.fatal,flog.error)
    logger(msg,name=loggername)
    for (detail in names(fargs))
      flog.debug(paste(detail,"="),fargs[[detail]],name=loggername,capture=TRUE)
    if (level %in% tracelevel) {
        calls <- sys.calls()
        calls <- calls[1:length(calls)-1]
        trace <- limitedLabels(c(calls, attr(obj, "calls")))
        if (length(trace) > 0L) {
          trace <- trace[length(trace):1L]
        }
        flog.debug("Traceback:",trace,
                   name=loggername,capture=TRUE)
    }

    ## Muffle any redundant output of the same message
    optionalRestart <- function(r) {
      res <- findRestart(r)
      if (!is.null(res)) invokeRestart(res)
    }
    optionalRestart("muffleMessage")
    optionalRestart("muffleWarning")
    if (level %in% c("ERROR","FATAL"))
      invokeRestart("tryError",msg,obj)
  }
  withRestarts(
      withCallingHandlers(expr,
                          debug=handler, message=handler, warning=handler,
                          caughtError=handler, error=handler),
      tryError=
        function(msg,obj)
          invisible(structure(msg, class = "try-error", condition = obj)))
}


shinyAppender <-
  setRefClass("shinyAppender",
              fields=c(file="character",
                       field="character",
                       messages="data.frame"),
              methods=list(
                  initialize = function (file="",field="",
                                         messages=data.frame(
                                             Messages=character()))
                  {
                    callSuper(file=file, field=field, steps=steps,
                              messages=messages)
                  },
                  update = function (line)
                  {
                    if (length(file) == 1L && nchar(file)>0L) {
                      cat(line, file = file, append = TRUE, sep = "")
                    }
                    messages$Messages <<- c(messages$Messages,line)
                    if (length(field)==1L && nchar(field)>0L) {
                      output[[field]] <- renderTable(messages,colnames=FALSE)
                    }
                  }
              ))





