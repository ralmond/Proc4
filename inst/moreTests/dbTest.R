###

m1 <- P4Message("Fred","Task1","PP","Task Done",
                details=list("Selection"="B"))
m2 <- P4Message("Fred","Task1","EI","New Obs",
                details=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- P4Message("Fred","Task1","EA","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

testcol <- mongo("Messages",
                 url="mongodb://test:secret@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages

m1 <- saveMess(m1,testcol)
m2 <- saveMess(m2,testcol)
m3 <- saveMess(m3,testcol)

## Test update logic
m1@data$time <- list("sec"=25.4)
m1 <- saveMess(m1,testcol)



m1a <- getMESSbyID(m1@"_id",testcol)
stopifnot(all.equal(m1,m1a))

buildMessQuery("Fred")
cat(buildMessQuery("Fred",sender=c("EI","EA")),"\n")
cat(buildMessQuery("Fred",before=Sys.time()),"\n")
cat(buildMessQuery("Fred",after=Sys.time()-as.difftime(1,units="hours")),"\n")

