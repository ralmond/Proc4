test_that("Mongo Message Queue", {
})

test_that("List Message Queue", {
})

test_that("fetchNextMessage Mongo", {
})
test_that("fetchNextMessage List", {
   messy <- list(
    P4Message("test","Test 1","Tester","Test Message"),
    P4Message("test","Test 2","Tester","Test Message",processed=TRUE),
    P4Message("test","Test 3","Tester","Test Message"))
  messq <- new("ListQueue","Qtest",messy)
  m1 <- fetchNextMessage(messq)
  expect_equal(context(m1),"Test 1")
  m2 <- fetchNextMessage(messq)
  expect_equal(context(m2),"Test 1")
  markAsProcessed(messq,m2)
  print(messq$messages)
  m3<-fetchNextMessage(messq)
  expect_equal(context(m3),"Test 3")
  markAsProcessed(messq,m3)
  m4<-fetchNextMessage(messq)
  expect_null(m4)
})

test_that("markAsProcessed Mongo", {
})
test_that("markAsProcessed List", {
})

test_that("markAsError Mongo", {
})
test_that("markAsError List", {
})


test_that("cleanMessageQueue", {
})

test_that("importMessages", {
})

test_that("resetProcessedMessages Mongo", {
})

test_that("resetProcessedMessages List", {
})
