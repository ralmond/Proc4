

mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EABN",mess="Statistics",
                   details=list("Physics_EAP"=0.5237,"Physics_Mode"="High"))


test_that("Capture Listener Captures Message", {
  cl <- CaptureListener()
  receiveMessage(cl,mess1)
  expect_equal(mess1,cl$lastMessage())
})
