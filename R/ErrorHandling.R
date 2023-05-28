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

## Breaks futile.logger line into its parts.
parseline <- function(line) {
  bracket1 <- regexpr("[",line,fixed=TRUE)
  bracket2 <- regexpr("]",line,fixed=TRUE)
  list(level=trimws(substr(line,1,bracket1-1)),
       time=strptime(substr(line,bracket1+1,bracket2-1),format="%Y-%m-%d %H:%M:%S"),
       message=trimws(substring(line,bracket2+1)))
}

## Forces Date into Mongo Format.
mongoDate <- function (dtime) {
  jsonlite::fromJSON(jsonlite::toJSON(mongo::unboxer(dtime),POSIXt="mongo"),FALSE)
}

mongoAppender <-
  setRefClass("mongoAppender",
              fields=c(db="JSONDB",
                       app="character",
                       engine="character",
                       tee="ANY"),
              methods=c(
                logit = function(line) {
                  pline <- parseline(line)
                  entry <- buildJQuery(app=app,engine=engine,level=pline$level,
                                       timestamp=pline$time,
                                       message=pline$message)
                  mdbInsert(db,entry)
                  if (is(tee,"connection") || is.character(tee)) {
                    cat(line,file=tee,append=TRUE,sep="")
                  }
                },
                logger = function() {
                  function(line) {.self$logit(line)}
                }))


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
                  update = function (line, output, renderer=function(tab) shiny::renderTable(tab,colname=FALSE))
                  {
                    if (length(file) == 1L && nchar(file)>0L) {
                      cat(line, file = file, append = TRUE, sep = "")
                    }
                    messages$Messages <<- c(messages$Messages,line)
                    if (length(field)==1L && nchar(field)>0L) {
                      # Call the render for a test case.
                      if (is.null(output)) do.call(renderer,list(messages))
                    } else {
                      output[[field]] <- do.call(renderer,list(messages))
                    }
                  },
                  logger = function() {
                    function (line) {
                      .self$update(line)
                    }
                  }
              ))





