test_that("isListener", {
  expect_false(isListener(NULL))
})

test_that("ListenerSet Constructor",{
  cl1 <- CaptureListener("Test1")
  cl2 <- CaptureListener("Test2")
  ls <- ListenerSet("Tester",list(Test1=cl1,Test2=cl2),
                    mongo::MongoDB("Messages",noMongo=TRUE),
                    mongo::MongoDB("Output",noMong=TRUE))
  expect_true(isListener(ls))
  expect_s4_class(ls,"ListenerSet")
  expect_equal(ls$sender,"Tester")
  expect_equal(listenerName(ls$listeners[["Test1"]]),"Test1")
  expect_equal(listenerName(ls$listeners[["Test2"]]),"Test2")
  expect_equal(ls$messdb()$colname,"Messages")
  expect_equal(ls$admindb()$colname,"Output")
})
test_that("ListenerSet notifyListeners",{
  cl1 <- CaptureListener("Test1")
  cl2 <- CaptureListener("Test2")
  mess1 <- P4Message(app="default", uid="Phred", context="Down Hill", mess="Statistics",
                     details=list("Physics_EAP"=0.5237, "Physics_Mode"="High"))
  mess1l <- as.jlist(mess1,attributes(mess1))
  mess1l$"_id" <- "0123"
  ## saveRec() calls iterate to get the m_id of the newly saved record
  mdb <- fake_mongo(iterate=list(iterator(list(mess1l))))

  ls <- ListenerSet("Tester",list(Test1=cl1,Test2=cl2),
                    mdb,
                    mongo::MongoDB("Output",noMong=TRUE))

  mess1 <- P4Message(app="default", uid="Phred", context="Down Hill", mess="Statistics",
                     details=list("Physics_EAP"=0.5237, "Physics_Mode"="High"))
  notifyListeners(ls,mess1)
  insertCall <- mdb$getLog(FALSE)[[1]]
  expect_equal(insertCall$op,"insert")
  m1 <- parse.json(insertCall$data,buildMessage)
  expect_equal(uid(m1),"Phred")
  expect_equal(sender(m1),"Tester")
  expect_length(cl1$messages,1L)
  expect_s4_class(cl1$lastMessage(),"P4Message")
  expect_equal(context(cl1$lastMessage()),"Down Hill")
  expect_equal(sender(cl1$lastMessage()),"Tester")
  expect_length(cl2$messages,1L)

})
test_that("ListenerSet addRemoveListeners",{
  cl1 <- CaptureListener("Test1")
  cl2 <- CaptureListener("Test2")
  ls <- ListenerSet("Tester",list(Test1=cl1),
                    mongo::MongoDB("Messages",noMongo=TRUE),
                    mongo::MongoDB("Output",noMong=TRUE))

  expect_true(isListener(ls$listeners[["Test1"]]))
  expect_false(isListener(ls$listeners[["Test2"]]))

  ls$addListener("Test2",cl2)
  expect_true(isListener(ls$listeners[["Test1"]]))
  expect_true(isListener(ls$listeners[["Test2"]]))

  ls$removeListener("Test1")
  expect_false(isListener(ls$listeners[["Test1"]]))
  expect_true(isListener(ls$listeners[["Test2"]]))


})
test_that("ListenerSet resetListener",{
  cl1 <- CaptureListener("Test1")
  cl2 <- CaptureListener("Test2")
  mess1 <- P4Message(app="default", uid="Phred", context="Down Hill", mess="Statistics",
                     details=list("Physics_EAP"=0.5237, "Physics_Mode"="High"))
  mess1l <- as.jlist(mess1,attributes(mess1))
  mess1l$"_id" <- "0123"
  ## saveRec() calls iterate to get the m_id of the newly saved record
  mdb <- fake_mongo(iterate=list(iterator(list(mess1l))))

  ls <- ListenerSet("Tester",list(Test1=cl1,Test2=cl2),
                    mdb,
                    mongo::MongoDB("Output",noMong=TRUE))

  mess1 <- P4Message(app="default", uid="Phred", context="Down Hill", mess="Statistics",
                     details=list("Physics_EAP"=0.5237, "Physics_Mode"="High"))
  notifyListeners(ls,mess1)
  expect_equal(mdb$getLog(FALSE)[[1]]$op,"insert")
  expect_length(cl1$messages,1L)
  expect_length(cl2$messages,1L)

  mdb$resetLog()
  resetListeners(ls,c("Self","Test1"),"default")
  expect_equal(mdb$lastLog()$op,"remove")
  expect_equal(mdb$lastLog()$query,buildJQuery(app="default"))
  expect_length(cl1$messages,0L)
  expect_length(cl2$messages,1L)

})

test_that("ListenerSet registerOutput",{
  cl1 <- CaptureListener("Test1")
  cl2 <- CaptureListener("Test2")
  adb <- mongo::fake_mongo(count=list(0L,1L))
  ls <- ListenerSet("Tester",list(Test1=cl1,Test2=cl2),
                    mongo::MongoDB("Messages",noMongo=TRUE),adb)

  ls$registerOutput("Scores","Scores.csv","default","TestEngine",
                    doc="Score table")
  expect_equal(adb$lastLog()$op,"insert")
  rec <- jsonlite::fromJSON(adb$lastLog()$data,FALSE)
  expect_equal(rec$app,"default")
  expect_equal(rec$process,"TestEngine")
  expect_equal(rec$type,"data")
  expect_equal(rec$name,"Scores")
  expect_equal(rec$filename,"Scores.csv")
  expect_s3_class(try(as.POSIXlt(rec$timestamp)),"POSIXt")
  expect_equal(rec$doc,"Score table")

  ls$registerOutput("Scores","Scores1.csv","default","TestEngine",
                    doc="Score table")
  expect_equal(adb$lastLog()$op,"update")
  expect_equal(adb$lastLog()$query,
    buildJQuery(app="default",name="Scores", process="TestEngine"))

})
