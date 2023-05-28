test_that("Upsert Listener Constructor", {
  ul <- UpsertListener(name="Sample",
                       messSet =c("Hello","Goodbye"),
                       qfields="app")
  expect_s4_class(ul,"UpsertListener")
  expect_setequal(ul$messSet,c("Hello","Goodbye"))
  expect_equal(ul$qfields,"app")
  expect_false(mdbAvailable(ul$messdb()))
})

test_that("Upsert Listener name", {
  ul <- UpsertListener("tester")
  expect_true(isListener(ul))
  expect_equal(listenerName(ul),"tester")
})

test_that("Upsert Listener Receive Message", {
  fm <- mongo::fake_mongo()
  ul <- UpsertListener("tester", db=fm,messSet=c("Scored Response"))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXlt("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  fm$resetLog()
  receiveMessage(ul,mess1)
  expect_equal(fm$lastLog()$op,"upsert")
  expect_equal(fm$lastLog()$query,buildJQuery(app=app(mess1),uid=uid(mess1)))
  expect_equal(fm$lastLog()$update,mongo::as.json(mess1))
})
test_that("Upsert Listener Receive Message Mongo", {
  testthat::skip_on_cran()
  if (is(try (mongolite::mongo()), "try-error"))
    testthat::skip("Mongo Not installed")

  mon <- mongo::MongoDB("TestEvents")
  withr::defer(mdbDrop(mon))
  mdbDrop(mon)
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  expect_equal(mdbCount(mon),0L)
  ul <- UpsertListener("tester", db=mon,messSet=c("Scored Response"))
  receiveMessage(ul,mess1)
  expect_equal(mdbCount(mon),1L)

  ## Replace
  details(mess1)$correct <- FALSE
  timestamp(mess1) <- as.POSIXct("2018-11-04 21:15:35 EST")
  receiveMessage(ul,mess1)
  expect_equal(mdbCount(mon),1L)

  ## insert
  uid(mess1) <- "Phred"
  receiveMessage(ul,mess1)
  expect_equal(mdbCount(mon),2L)

})
test_that("Upsert Listener ignore Message", {
  fm <- mongo::fake_mongo()
  ul <- UpdateListener("tester",db=fm,messSet=c("Unscored Response"))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  fm$resetLog()
  receiveMessage(ul,mess1)
  expect_length(fm$getLog(),0L)
})

test_that("Upsert Listener reset Message", {
  fm <- mongo::fake_mongo()
  ul <- UpsertListener("tester",db=fm,messSet=c("Unscored Response"))
  fm$resetLog()
  clearMessages(ul,"testapp")
  expect_equal(fm$lastLog()$op,"remove")
  expect_equal(fm$lastLog()$query,'{ "app":"testapp" }')
})
upconfig <-
  jsonlite::fromJSON('{
  "name":"ToEA",
  "type":"UpsertListener",
  "dbname":"EARecords",
  "colname":"EvidenceSets",
  "messages":["New Observables"]
}',FALSE)
test_that("Upsert Listener buildListener",{
  l2 <- buildListener(upconfig,"test",dburi=character())
  expect_true(isListener(l2))
  expect_equal(listenerName(l2),"ToEA")
  expect_s4_class(l2,"UpsertListener")
  expect_true(any(grepl("New Observables",listeningFor(l2))))
})

