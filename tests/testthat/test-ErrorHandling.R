setRefClass("appender.list",c(log="list"),
            methods=list(
              initialize=function(...) {
                callSuper(log=list(),...)
              },
              flog = function(line) {
                log <<- c(list(line),log)
              },
              ## Need this as lambda.R does recognize refMethodDef as a function.
              flogger = function() {
                function(line) {
                  .self$flog(line)
                }
              },
              last = function() {
                if (length(log) > 0L) return(log[[1]])
                else return(NULL)
              },
              reset=function(){
                log <<- list()
              }))
logObj <- new("appender.list")
futile.logger::flog.appender(logObj$flogger())

test_that("withFlogging error", {
  logObj$reset()
  futile.logger::flog.appender(logObj$flogger())
  futile.logger::flog.threshold("ERROR")
  withFlogging(stop("Test Error"))
  expect_length(logObj$log,1L)
  expect_match(logObj$last(),"FATAL .* While stop\\(\"Test Error\"\\), *FATAL ERROR: *Test Error")

  ## Test traceback
  logObj$reset()
  futile.logger::flog.threshold("DEBUG")
  withFlogging(stop("Test Error"))
  expect_length(logObj$log,2L)
  expect_match(logObj$log[[1]][1], "DEBUG .* Traceback:")
  expect_match(logObj$log[[2]],"FATAL .* While stop\\(\"Test Error\"\\), *FATAL ERROR: *Test Error")

})


test_that("withFlogging warn", {
  logObj$reset()
  futile.logger::flog.appender(logObj$flogger())
  futile.logger::flog.threshold("ERROR")
  withFlogging(warning("Test warning"))
  expect_length(logObj$log,0L)

  futile.logger::flog.threshold("WARN")
  withFlogging(warning("Test warning"))
  expect_length(logObj$log,1L)
  expect_match(logObj$last(),"WARN .* While warning\\(\"Test warning\"\\), *WARN: *Test warning")

  ## Test traceback
  logObj$reset()
  futile.logger::flog.threshold("DEBUG")
  withFlogging(warning("Test warning"))
  expect_length(logObj$log,2L)
  expect_match(logObj$log[[1]][1], "DEBUG .* Traceback:")
  expect_match(logObj$log[[2]],"WARN .* While warning\\(\"Test warning\"\\), *WARN: *Test warning")

})

test_that("withFlogging info", {
  logObj$reset()
  futile.logger::flog.appender(logObj$flogger())
  futile.logger::flog.threshold("WARN")
  withFlogging({flog.info("Information");stop("Test warning")},context="Info Test 1")
  expect_length(logObj$log,1L)
  expect_match(logObj$last(),"^FATAL .* While Info Test 1")

  logObj$reset()
  futile.logger::flog.threshold("INFO")
  withFlogging({flog.info("Information");stop("Test warning")},context="Info Test 2")
  expect_length(logObj$log,2L)
  expect_match(logObj$log[[1]],"^FATAL .* While Info Test 2")
  expect_match(logObj$log[[2]],"^INFO .* Information")

})
test_that("withFloggin debug",{
  logObj$reset()
  futile.logger::flog.appender(logObj$flogger())
  futile.logger::flog.threshold("INFO")
  withFlogging({flog.debug("Current state");stop("Test warning")},context="Debug Test 1")
  expect_length(logObj$log,1L)
  expect_match(logObj$last(),"^FATAL .* While Debug Test 1")

  logObj$reset()
  futile.logger::flog.threshold("DEBUG")
  withFlogging({flog.debug("Current state");stop("Test warning")},context="Debug Test 2")
  expect_length(logObj$log,3L)
  expect_match(logObj$log[[1]][1],"DEBUG .* Traceback:")
  expect_match(logObj$log[[2]],"^FATAL .* While Debug Test 2")
  expect_match(logObj$log[[3]],"^DEBUG .* Current state")
})

test_that("withFlogging trace", {
  logObj$reset()
  futile.logger::flog.appender(logObj$flogger())
  futile.logger::flog.threshold("DEBUG")
  withFlogging({flog.trace("At checkpoint 1");stop("Test warning")},context="Trace Test 1")
  expect_length(logObj$log,2L)
  expect_match(logObj$log[[1]][1],"DEBUG .* Traceback:")
  expect_match(logObj$log[[2]],"^FATAL .* While Trace Test 1")

  logObj$reset()
  futile.logger::flog.threshold("TRACE")
  withFlogging({flog.trace("At checkpoint 1");stop("Test warning")},context="Trace Test 2")
  expect_length(logObj$log,3L)
  expect_match(logObj$log[[1]][1],"DEBUG .* Traceback:")
  expect_match(logObj$log[[2]],"^FATAL .* While Trace Test 2")
  expect_match(logObj$log[[3]],"^TRACE .* At checkpoint 1")
})

test_that("mongoAppender",{
  fm <- mongo::fake_mongo()
  tmpfile <- tempfile("testlog",fileext=".log")
  print(tmpfile)
  apnd <- mongoAppender(db=fm,app="p4test",engine="Tester",tee=tmpfile)
  futile.logger::flog.appender(apnd$logger(),"TEST")
  futile.logger::flog.threshold("TRACE","TEST")
  futile.logger::flog.error("An Error",name="TEST")
  expect_equal(fm$lastLog()$op,"insert")
  entry <- jsonlite::fromJSON(fm$lastLog()$data,FALSE)
  expect_equal(entry$app,"p4test")
  expect_equal(entry$engine,"Tester")
  expect_equal(entry$level,"ERROR")
  expect_equal(names(entry$timestamp),"$date")
  expect_equal(entry$message,"An Error")

  futile.logger::flog.warn("A Warning",name="TEST")
  expect_equal(fm$lastLog()$op,"insert")
  entry <- jsonlite::fromJSON(fm$lastLog()$data,FALSE)
  expect_equal(entry$level,"WARN")

  futile.logger::flog.info("Some Info",name="TEST")
  expect_equal(fm$lastLog()$op,"insert")
  entry <- jsonlite::fromJSON(fm$lastLog()$data,FALSE)
  expect_equal(entry$level,"INFO")

  futile.logger::flog.debug("Debugging Info",name="TEST")
  expect_equal(fm$lastLog()$op,"insert")
  entry <- jsonlite::fromJSON(fm$lastLog()$data,FALSE)
  expect_equal(entry$level,"DEBUG")

  futile.logger::flog.trace("Residue",name="TEST")
  expect_equal(fm$lastLog()$op,"insert")
  entry <- jsonlite::fromJSON(fm$lastLog()$data,FALSE)
  expect_equal(entry$level,"TRACE")

  entries <- readLines(tmpfile,n=10,ok=TRUE)
  expect_length(entries,5L)

})


