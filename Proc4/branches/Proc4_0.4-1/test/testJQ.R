
time1 <- as.POSIXct("2018-08-16 19:12:19 EDT")
time1l <- as.POSIXlt("2018-08-16 19:12:19 EDT")
time1.mongo <- 1534461139000
time2 <- as.POSIXct("2018-08-16 19:22:19 EDT")
time2l <- as.POSIXlt("2018-08-16 19:22:19 EDT")
time1.mongo <- 1534461739000

## Low level test of the JQterm value options.

stopifnot(buildJQterm("uid","Fred")=='"uid":"Fred"')
stopifnot(buildJQterm("uid",c("Phred","Fred"))=='"uid":{"$in":["Phred","Fred"]}')
stopifnot(buildJQterm("time",time1)=='"time":{"$date":1534461139000}')
stopifnot(buildJQterm("time",time1l)=='"time":{"$date":1534461139000}')
stopifnot(buildJQterm("time",c(time1,time2))==
          '"time":{"$in":[{"$date":1534461139000},{"$date":1534461739000}]}')
stopifnot(buildJQterm("time",c(gt=time1))==
          '"time":{ "$gt":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(lt=time1))==
          '"time":{ "$lt":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(gte=time1))==
          '"time":{ "$gte":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(lte=time1))==
          '"time":{ "$lte":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(ne=time1))==
          '"time":{ "$ne":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(eq=time1))==
          '"time":{ "$eq":{"$date":1534461139000} }')
stopifnot(buildJQterm("time",c(gt=time1,lt=time2))==
          '"time":{ "$gt":{"$date":1534461139000}, "$lt":{"$date":1534461739000} }')
stopifnot(buildJQterm("count",c(nin=1,2:4))==
          '"count":{"$nin":[1,2,3,4]}')
stopifnot(buildJQterm("count",c("in"=1,2:4))==
          '"count":{"$in":[1,2,3,4]}')
stopifnot(buildJQterm("count",c(ne=1,ne=5))==
          '"count":{ "$ne":1, "$ne":5 }')

#cat(buildJQterm("count",c(ne=1,ne=5)),"\n")


stopifnot(buildJQuery(app="default",uid="Phred")==
          '{ "app":"default", "uid":"Phred" }')
stopifnot(buildJQuery("_id"=c(oid="123456789"))==
          '{ "_id":{ "$oid":"123456789" } }')
stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5))==
          '{ "name":"George", "count":{ "$gt":3, "$lt":5 } }')
stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5),
                      rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))==
          '{ "name":"George", "count":{ "$gt":3, "$lt":5 }, "$limit":1, "$sort":{timestamp:-1} }')


