test_that("Capture Listener name", {
  cl <- CaptureListener("tester")
  expect_s4_class(cl,"CaptureListener")
  expect_true(isListener(cl))
  expect_equal(listenerName(cl),"tester")
})

test_that("Capture Listener Receive Message", {
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EABN",mess="Statistics",
                     details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
  cl <- CaptureListener()
  receiveMessage(cl,mess1)
  expect_true(all.equal(mess1,cl$lastMessage()))

  cl2 <- CaptureListener(messSet="Statistics")
  receiveMessage(cl2,mess1)
  expect_true(all.equal(mess1,cl2$lastMessage()))

  cl3 <- CaptureListener(messSet="Observables")
  receiveMessage(cl3,mess1)
  expect_null(cl3$lastMessage())

})

test_that("Capture Listener reset",{
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EABN",mess="Statistics",
                     details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))
  cl <- CaptureListener()
  receiveMessage(cl,mess1)
  expect_length(cl$messages,1L)
  receiveMessage(cl,mess1)
  expect_length(cl$messages,2L)
  clearMessages(cl,"default")
  expect_length(cl$messages,0L)

})

clconfig <-
jsonlite::fromJSON('{
  "name":"TestConnection",
  "type":"CaptureListener"
}',FALSE)

test_that("Capture Listener buildListener",{
  cl <- buildListener(clconfig,"test",character())
  expect_true(isListener(cl))
  expect_s4_class(cl,"CaptureListener")
  expect_equal(listenerName(cl),"TestConnection")
})

test_that("RefListener listenerDataType",{

})

test_that("RefListener listenerDataFname",{

})

