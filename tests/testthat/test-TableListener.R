test_that("Table Listener Constructor", {
  fl <-c(uid="character", context="character",
         timestamp="character",
         solvedtime="numeric",
         trophy="ordered(none,silver,gold)")
  tabMaker <- TableListener(name="Trophy Table",
                            messSet="New Observables",
                            fieldlist=fl)
  expect_s4_class(tabMaker,"TableListener")
  expect_equal(tabMaker$messSet,"New Observables")
  expect_setequal(names(tabMaker$fieldlist),names(fl))
  tabMaker$initDF()
  df <- tabMaker$returnDF()
  expect_setequal(names(df),names(fl))
})

test_that("Table Listener name", {
  tl <- TableListener("tester")
  expect_true(isListener(tl))
  expect_equal(listenerName(tl),"tester")
})

test_that("Table Listener Receive Message", {
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EIEvent",mess="New Observables",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     details=list(trophy="gold",solvedtime=10))
  mess2 <- P4Message(app="default",uid="Phred",context="Around the Tree",
                     sender="EIEvent",mess="New Observables",
                     as.POSIXct("2018-11-04 21:16:35 EST"),
                     details=list(trophy="silver",solvedtime=25))
  tabMaker <- TableListener(name="Trophy Table",
                            messSet="New Observables",
                            fieldlist=c(uid="character", context="character",
                                        timestamp="character",
                                        solvedtime="numeric",
                                        trophy="ordered(none,silver,gold)"))

  receiveMessage(tabMaker,mess1)
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),1L)

  receiveMessage(tabMaker,mess2)
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),2L)
  expect_equal(result$context,c("Down Hill","Around the Tree"))
  expect_true(result$trophy[1] > result$trophy[2])

})

test_that("Table Listener ignore Message", {
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EIEvent",mess="New Observables",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     details=list(trophy="gold",solvedtime=10))
  mess2 <- P4Message("Fred","Task 1","Evidence ID","Scored Response",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     list(correct=TRUE,selection="D"))
  tabMaker <- TableListener(name="Trophy Table",
                            messSet="New Observables",
                            fieldlist=c(uid="character", context="character",
                                        timestamp="character",
                                        solvedtime="numeric",
                                        trophy="ordered(none,silver,gold)"))

  receiveMessage(tabMaker,mess1)
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),1L)

  receiveMessage(tabMaker,mess2)
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),1L)
})

test_that("Table Listener reset", {
  mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                     sender="EIEvent",mess="New Observables",
                     as.POSIXct("2018-11-04 21:15:25 EST"),
                     details=list(trophy="gold",solvedtime=10))
  mess2 <- P4Message(app="default",uid="Phred",context="Around the Tree",
                     sender="EIEvent",mess="New Observables",
                     as.POSIXct("2018-11-04 21:16:35 EST"),
                     details=list(trophy="silver",solvedtime=25))
  tabMaker <- TableListener(name="Trophy Table",
                            messSet="New Observables",
                            fieldlist=c(uid="character", context="character",
                                        timestamp="character",
                                        solvedtime="numeric",
                                        trophy="ordered(none,silver,gold)"))
  receiveMessage(tabMaker,mess1)
  receiveMessage(tabMaker,mess2)
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),2L)

  clearMessages(tabMaker,"default")
  result <- tabMaker$returnDF()
  expect_equal(nrow(result),0L)
})

tlconfig <-
jsonlite::fromJSON('{
 "name":"ppLS<app>",
 "type":"TableListener",
 "messages":["Coins Earned","Coins Spent", "LS Watched"],
 "fields":{
    "uid":"character",
    "context":"character",
    "timestamp":"character",
    "currentMoney":"numeric",
    "appId":"numeric",
    "mess":"character",
    "money":"numeric",
    "onWhat":"character",
    "LS_duration":"difftime",
    "learningSupportType":"character"
 }
}',FALSE)

test_that("TableListner builder",{
  l1 <- buildListener(tlconfig,"test",dburi=character())
  expect_true(isListener(l1))
  expect_equal(listenerName(l1),"ppLStest")
  expect_s4_class(l1,"TableListener")
  expect_true(any(grepl("Coins Spent",l1$messSet)))
  l1$initDF()
  df <- l1$returnDF()
  expect_setequal(names(df),c("uid", "context", "timestamp", "currentMoney",
                              "appId", "mess", "money", "onWhat",
                              "LS_duration", "learningSupportType"))

})

test_that("Table Listener listenerDataTable", {
})
