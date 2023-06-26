test_that("Injection Listener Constructor", {
  il <- InjectionListener(name="Sample",
                          messSet =c("Hello","Goodbye"))
  expect_s4_class(il,"InjectionListener")
  expect_setequal(il$messSet,c("Hello","Goodbye"))
  expect_false(mdbAvailable(il$messdb()))
})

test_that("Injection Listener name", {
  il <- InjectionListener("tester")
  expect_true(isListener(il))
  expect_equal(listenerName(il),"tester")
})

test_that("Injection Listener Receive Message", {
  fm <- mongo::fake_mongo()
  il <- InjectionListener("tester", db=fm,messSet=c("Scored Response"))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXlt("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  fm$resetLog()
  receiveMessage(il,mess1)
  expect_equal(fm$lastLog()$op,"insert")
  expect_equal(fm$lastLog()$data,mongo::as.json(mess1))
})

test_that("Injection Listener Receive Message Mongo", {
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
  il <- InjectionListener("tester", db=mon,messSet=c("Scored Response"))
  receiveMessage(il,mess1)
  expect_equal(mdbCount(mon),1L)
})

test_that("Injection Listener ignore Message", {
  fm <- mongo::fake_mongo()
  il <- InjectionListener("tester",db=fm,messSet=c("Unscored Response"))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  fm$resetLog()
  receiveMessage(il,mess1)
  expect_length(fm$log,0L)
})

test_that("Injection Listener reset", {
  fm <- mongo::fake_mongo()
  il <- InjectionListener("tester",db=fm,messSet=c("Unscored Response"))
  fm$resetLog()
  clearMessages(il,"testapp")
  expect_equal(fm$lastLog()$op,"remove")
  expect_equal(fm$lastLog()$query,'{ "app":"testapp" }')
})

ilconfig <-
jsonlite::fromJSON('{
  "name":"ToEA",
  "type":"InjectionListener",
  "dbname":"EARecords",
  "colname":"EvidenceSets",
  "messages":["New Observables"]
}',FALSE)

test_that("Injection Listener buildListener",{
  l2 <- buildListener(ilconfig,"test",dburi=character())
  expect_true(isListener(l2))
  expect_equal(listenerName(l2),"ToEA")
  expect_s4_class(l2,"InjectionListener")
  expect_true(any(grepl("New Observables",listeningFor(l2))))
})

test_that("Injection Listener listenerDataTable", {
})

