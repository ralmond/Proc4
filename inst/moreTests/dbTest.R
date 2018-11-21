###
library(Proc4)

m1 <- P4Message("Fred","Task1","PP","Task Done",
                details=list("Selection"="B"))
m2 <- P4Message("Fred","Task1","EI","New Obs",
                details=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- P4Message("Fred","Task1","EA","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

testcol <- mongo("Messages",
                 url="mongodb://test:S3cr3t@127.0.0.1:27017/test")
## Mongodb is the protocol
## user=test, password =secret
## Host = 127.0.0.1 -- localhost
## Port = 27017 -- Mongo default
## db = test
## collection = Messages
## Execute in Mongo Shell
## db.createUser({
## ... user: "test",
## ... pwd: "S3cr3t",
## ... roles: [{role: "readWrite", db: "test"}]
## ... });

m1 <- saveRec(m1,testcol)
m2 <- saveRec(m2,testcol)
m3 <- saveRec(m3,testcol)

## Test update logic
m1@data$time <- list(tim=25.4,units="secs")
m1 <- saveRec(m1,testcol)


m1a <- getOneRec(buildJQuery("_id"=c(oid=m1@"_id")),testcol,parseMessage)
stopifnot(all.equal(m1,m1a))

m123 <- getManyRecs(buildJQuery(uid="Fred"),testcol,parseMessage)
stopifnot(all.equal(m1,m123[[1]]),
          all.equal(m2,m123[[2]]),
          all.equal(m3,m123[[3]],tolerance=.001))
m23 <- getManyRecs(buildJQuery(uid="Fred",sender=c("EI","EA")),
                   testcol,parseMessage)
stopifnot(all.equal(m2,m23[[1]]),
          all.equal(m3,m23[[2]],tolerance=.001))
m321 <- getManyRecs(buildJQuery(uid="Fred",timestamp=c(lte=Sys.time())),
            testcol,parseMessage,sort=c(timestamp=-1))
stopifnot(all.equal(m3,m321[[1]],tolerance=.001),
          all.equal(m2,m321[[2]]),
          all.equal(m1,m321[[3]]))
getManyRecs(buildJQuery(uid="Fred",
                        timestamp=c(gte=Sys.time()-as.difftime(1,units="hours"))),
                        testcol,parseMessage)

