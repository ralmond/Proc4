test_that("Update Listener Constructor", {
  ul <- UpdateListener(name="Sample",
                       messSet =c("Hello","Goodbye"),
                       targetField="details",
                       qfields="app",
                       jsonEncoder="toJSON")
  expect_s4_class(ul,"UpdateListener")
  expect_setequal(ul$messSet,c("Hello","Goodbye"))
  expect_equal(ul$targetField,"details")
  expect_equal(ul$qfields,"app")
  expect_equal(ul$jsonEncoder,"toJSON")
  expect_false(mdbAvailable(ul$messdb()))
})

test_that("Update Listener name", {
  ul <- UpdateListener("tester")
  expect_true(isListener(ul))
  expect_equal(listenerName(ul),"tester")
})

test_that("Update Listener Receive Message", {

  fm <- fake_mongo(count=list(0L,1L))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  ul <- UpdateListener("tester", db=fm,messSet=c("Scored Response"))

  ## New message, insert
  receiveMessage(ul,mess1)
  qq <- buildJQuery(app=app(mess1),uid=uid(mess1))
  #Count
  countop <- fm$getLog(FALSE)[[1]]
  expect_equal(countop$op,"count")
  expect_equal(countop$query,qq)
  #Insert
  insertop <- fm$getLog(FALSE)[[2]]
  expect_equal(insertop$op,"insert")
  expect_equal(insertop$data,as.json(mess1))
  #Update
  updateop <- fm$getLog(TRUE)[[1]]
  expect_equal(updateop$op,"update")
  expect_equal(updateop$query,qq)
  update <- jsonlite::fromJSON(updateop$update,FALSE)
  expect_equal(names(update),"$set")
  expect_setequal(names(update[[1]]),c("data","context","timestamp"))

  ## Modified message, update.
  details(mess1)$correct=FALSE
  timestamp(mess1) <- as.POSIXct("2018-11-04 21:15:30 EST")
  fm$resetLog()
  receiveMessage(ul,mess1)
  #Count
  countop <- fm$getLog(FALSE)[[1]]
  expect_equal(countop$op,"count")
  expect_equal(countop$query,qq)

  #Update
  updateop <- fm$getLog(TRUE)[[1]]
  expect_equal(updateop$op,"update")
  expect_equal(updateop$query,qq)
  update <- jsonlite::fromJSON(updateop$update,FALSE)
  expect_equal(names(update),"$set")
  expect_setequal(names(update[[1]]),c("data","context","timestamp"))

})
data2json <- function(dat) {
  jsonlite::toJSON(mongo::unboxer(dat))
}
test_that("Update Listener RecieveMessage, no target field", {

  fm <- fake_mongo(count=list(0L,1L))
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EIEvent",mess="Money Earned",
                     details=list(trophyHall=list(list("Down Hill"="gold")),
                                  bankBalance=10))
  mess2 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EIEvent",mess="Money Earned",
                     details=list(trophyHall=list(list("Down Hill"="gold"),
                                                  list("Stairs"="silver")),
                                  bankBalance=20))

  ul <- UpdateListener("tester", targetField="", db=fm, messSet=c("Money Earned"),
                       jsonEncoder=data2json)
  ## New message, insert
  receiveMessage(ul,mess1)
  qq <- buildJQuery(app=app(mess1),uid=uid(mess1))
  #Count
  countop <- fm$getLog(FALSE)[[1]]
  expect_equal(countop$op,"count")
  expect_equal(countop$query,qq)
  #Insert
  insertop <- fm$getLog(FALSE)[[2]]
  expect_equal(insertop$op,"insert")
  expect_equal(insertop$data,as.json(mess1))
  #Update
  updateop <- fm$getLog(TRUE)[[1]]
  expect_equal(updateop$op,"update")
  expect_equal(updateop$query,qq)
  update <- jsonlite::fromJSON(updateop$update,FALSE)
  expect_equal(names(update),"$set")
  expect_setequal(names(update[[1]]),c("trophyHall","bankBalance"))

  ## Modified message, update.
  fm$resetLog()
  receiveMessage(ul,mess2)
  #Count
  countop <- fm$getLog(FALSE)[[1]]
  expect_equal(countop$op,"count")
  expect_equal(countop$query,qq)

  #Update
  updateop <- fm$getLog(TRUE)[[1]]
  expect_equal(updateop$op,"update")
  expect_equal(updateop$query,qq)
  update <- jsonlite::fromJSON(updateop$update,FALSE)
  expect_equal(names(update),"$set")
  expect_setequal(names(update[[1]]),c("trophyHall","bankBalance"))

})
test_that("Update Listener Receive Message Mongo", {
  testthat::skip_on_cran()
  if (is(try (mongolite::mongo()), "try-error"))
    testthat::skip("Mongo Not installed")

  mon <- mongo::MongoDB("TestEvents")
  #withr::defer(mdbDrop(mon))
  mdbDrop(mon)
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  ul <- UpdateListener("tester", db=mon,messSet=c("Scored Response"))

  expect_equal(mdbCount(mon),0L)
  receiveMessage(ul,mess1)
  expect_equal(mdbCount(mon),1L)

  details(mess1)$correct=FALSE
  timestamp(mess1) <- as.POSIXct("2018-11-04 21:15:30 EST")
  receiveMessage(ul,mess1)
  expect_equal(mdbCount(mon),1L)
  mess1a <- getOneRec(mon)
  expect_false(details(mess1a)$correct)
  expect_gt(timestamp(mess1),as.POSIXct("2018-11-04 21:15:25 EST"))

})

test_that("Update Listener ignore Message", {
  fm <- fake_mongo(count=list(0L,1L))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  ul <- UpdateListener("tester", db=fm,messSet=c("Statistics"))

  receiveMessage(ul,mess1)
  expect_null(fm$lastLog())

})
test_that("Update Listener reset Message", {
  fm <- fake_mongo(count=list(0L,1L))
  mess1 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  ul <- UpdateListener("tester", db=fm,messSet=c("Scored Response"))
  ul$reset("default")
  lm <- fm$lastLog()
  expect_equal(lm$op,"remove")
  expect_equal(lm$query,buildJQuery(app="default"))
})

ulconfig <-
  jsonlite::fromJSON('	{
	    "name":"PPPersistantData",
	    "type":"UpdateListener",
	    "dbname":"Proc4",
	    "colname":"Players",
	    "targetField":"data",
	    "jsonEncoder":"trophy2json",
	    "messages":["Money Earned", "Money Spent"]
	}',FALSE)

test_that("Update Listener buildListener",{
  l3 <- buildListener(ulconfig,"test",character())
  expect_true(isListener(l3))
  expect_equal(listenerName(l3),"PPPersistantData")
  expect_s4_class(l3,"UpdateListener")
  expect_true(any(grepl("Money Earned",l3$messSet)))
})

test_that("Update Listener listenerDataTable", {
})
