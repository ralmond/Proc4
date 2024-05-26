test_that("P4Message constructor", {
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  expect_s4_class(mess1,"P4Message")
  expect_true(is.na(mongo::m_id(mess1)))
  expect_equal(names(mongo::m_id(mess1)),"oid")
  expect_equal(app(mess1),"default")
  expect_equal(uid(mess1),"Fred")
  expect_equal(context(mess1),"Task 1")
  expect_equal(sender(mess1),"Evidence ID")
  expect_equal(mess(mess1),"Scored Response")
  expect_equal(timestamp(mess1), as.POSIXct("2018-11-04 21:15:25 EST"))
  expect_equal(details(mess1)$correct,TRUE)
  expect_equal(details(mess1)$selection,"D")
  expect_false(processed(mess1))
  expect_equal(processingError(mess1),"")
})

test_that("all.equal.P4Message",{
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"),"default",FALSE)
  expect_true(all.equal(mess1,mess1))
  mess2 <- P4Message("Phred","Task 2","EvidenceID","Unscored Response",
                     as.POSIXct("2018-11-04 21:1ar5:25 EST"),
                     list(correct=FALSE,choice="E"),"test",TRUE)
  mongo::m_id(mess2) <- "0123"

  expect_setequal(all.equal(mess1,mess2,check_ids=FALSE),
                  c("Application IDs do not match.", "User IDs do not match.", "Contexts do not match.",
                    "Senders do not match.", "Messages do not match.",
                    "Names or number of data differ.",
                    "Data in target but not in current: selection",
                    "Data in current but not in target: choice",
                    "Names: 1 string mismatch",
                    "Component \"correct\": 1 element mismatch",
                    "Component 2: 1 string mismatch"))
  expect_setequal(all.equal(mess1,mess2,checkTimestamp=TRUE,check_ids=TRUE),
                  c("Database IDs do not match.", "Application IDs do not match.",
                    "User IDs do not match.", "Contexts do not match.",
                    "Senders do not match.", "Messages do not match.",
                    "Timestamps differ by more than .1 secs",
                    "Names or number of data differ.",
                    "Data in target but not in current: selection",
                    "Data in current but not in target: choice",
                    "Names: 1 string mismatch",
                    "Component \"correct\": 1 element mismatch",
                    "Component 2: 1 string mismatch"))
})

test_that("P4Message fields", {
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  uid(mess1) <- "Phred"
  expect_equal(uid(mess1),"Phred")
  context(mess1) <- "Task 2"
  expect_equal(context(mess1),"Task 2")
  sender(mess1) <- "EID"
  expect_equal(sender(mess1),"EID")
  mess(mess1) <- "Raw Response"
  expect_equal(mess(mess1),"Raw Response")
  timestamp(mess1) <- as.POSIXct("2023-11-04 21:15:25 EST")
  expect_equal(timestamp(mess1), as.POSIXct("2023-11-04 21:15:25 EST"))
  details(mess1)$correct <- FALSE
  expect_equal(details(mess1)$correct,FALSE)
  details(mess1)$selection <- "E"
  expect_equal(details(mess1)$selection,"E")
  processed(mess1) <- TRUE
  expect_true(processed(mess1))
  e <- simpleCondition("Test message")
  processingError(mess1) <- e
  expect_equal(processingError(mess1),toString(e))

})

test_that("P4Message json", {
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  json <- mongo::as.json(mess1)
  mess2 <- buildMessage(jsonlite::fromJSON(json,FALSE))
  diff <- all.equal(mess1,mess2)
  expect(isTRUE(diff),
         paste("JSON round trip does not match:  ",paste(diff,collapse="\n"),sep="\n"))

})


test_that("markAsProcessed fake", {
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  mess1s <- mess1
  mongo::m_id(mess1s) <- "012"
  mess1p <- mess1s
  processed(mess1p) <- TRUE
  fm <- mongo::fake_mongo()
  fm$resetQue("iterate",list(mongo::iterator(jsonlite::fromJSON('[{"_id":"012"}]',FALSE))))
  mess1a <- saveRec(fm,mess1)
  expect_equal(mongo::m_id(mess1a),mongo::m_id(mess1p))

  mess1b <- markAsProcessed(fm,mess1a)
  expect_true(processed(mess1b))
  expect_equal(fm$lastLog()$op,"update")
  expect_equal(fm$lastLog()$update,'{"$set": {"processed":true}}')
})

test_that("markAsProcessed DB", {
  testthat::skip_on_cran()
  if (is(try (mongolite::mongo()), "try-error"))
    testthat::skip("Mongo Not installed")

  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))

  mon <- mongo::MongoDB("TestEvents")
  withr::defer(mdbDrop(mon))
  mdbDrop(mon)
  mess1a <- saveRec(mon,mess1)
  id <- m_id(mess1a)
  expect_false(is.na(id))
  expect_equal(mdbCount(mon,buildJQuery(processed=TRUE)),0L)

  mess1b <- markAsProcessed(mon,mess1a)
  expect_true(processed(mess1b))
  expect_equal(mdbCount(mon,buildJQuery(processed=TRUE)),1L)

  mess1c <- getOneRec(mon,buildJQuery("_id"=id))
  print(mess1c)
  expect_false(is.null(mess1c))
  expect_true(processed(mess1c))

})

test_that("markAsError", {
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  mess1s <- mess1
  mongo::m_id(mess1s) <- "012"
  mess1p <- mess1s
  processed(mess1p) <- TRUE
  fm <- mongo::fake_mongo()
  fm$resetQue("iterate",list(mongo::iterator(jsonlite::fromJSON('[{"_id":"012"}]',FALSE))))
  mess1a <- saveRec(fm,mess1)
  expect_equal(mongo::m_id(mess1a),mongo::m_id(mess1p))

  err <- simpleError("Test Condition, don't \"worry\"")

  mess1b <- markAsError(fm,mess1a,err)
  expect_false(is.null(processingError(mess1b)))
  expect_equal(fm$lastLog()$op,"update")
  expect_equal(fm$lastLog()$update,"{\"$set\": {\"pError\":\"Error: Test Condition, don't \\\"worry\\\"\\n\"}}")
})

test_that("markAsError DB", {
  testthat::skip_on_cran()
  if (is(try (mongolite::mongo()), "try-error"))
    testthat::skip("Mongo Not installed")

  mon <- mongo::MongoDB("TestEvents")
  withr::defer(mdbDrop(mon))
  mdbDrop(mon)
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  mess1a <- saveRec(mon,mess1)
  id <- m_id(mess1a)
  expect_false(is.na(id))
  ## Test for at least one entry in error list.
  ## https://stackoverflow.com/questions/7811163/query-for-documents-where-array-size-is-greater-than-1
  ##expect_equal(mdbCount(mon,'{"pError.0":{"$exists":true}}'),0L)
  expect_equal(mdbCount(mon,'{"pError":{"$ne":""}}'),0L)
  err <- simpleError("Test Condition, don't \"worry\"")

  mess1b <- markAsError(mon,mess1a,err)
  expect_false(is.null(processingError(mess1b)))
  expect_equal(mdbCount(mon,'{"pError":{"$ne":""}}'),1L)
  mess1c <- getOneRec(mon,buildJQuery("_id"=id))
  expect_false(is.null(mess1c))
  expect_equal(processingError(mess1c), "Error: Test Condition, don't \"worry\"\n")
})
