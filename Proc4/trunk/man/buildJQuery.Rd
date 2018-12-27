\name{buildJQuery}
\alias{buildJQuery}
\alias{buildJQterm}
\title{Transforms a query into JQuery JSON.}
\description{

  This function takes a query which is expressed in the argument list
  and transforms it into a JSON query document which can be used with
  the Mongo Database.  The function \code{buildJQterm} is a helper
  function which builds up a single term of the query.

}
\usage{
buildJQuery(..., rawfields = character())
buildJQterm(name,value)
}
\arguments{
  \item{\dots}{This should be a named list of arguments.  The values
    should be the desired query value, or a more complex expression (see
    details).
  }
  \item{rawfields}{
    These arguments are passed as character vectors directly into the
    query document without processing.
  }
  \item{name}{The name of the field.}
  \item{value}{The value of the field or an expression which gives a
    query for the resulting document.}
}
\details{

  A typical query to a Mongo database collection is done with a JSON
  object which has a number of bits that look like
  \dQuote{\emph{field}:\emph{value}}, where \emph{field} names a field in
  the document, and \emph{value} is a value to be matched.  A record
  matches the query if all of the fields specified in the query
  match the corresponding fields in the record.

  Note that \emph{value} could be a special expression which gives
  specifies a more complex expression allowing for ranges of values.
  In particular, the Mongo query language supports the following
  operators: \code{"$eq", "$ne", "$gt", "$lt", "$gte", "$lte"}.
  These can be specified using a value of the form
  \code{c(<op>=<value>)}, where \emph{op} is one of the mongo operators,
  without the leading \sQuote{$}.  Multiple op--value pairs can be specified;
  for example, \code{count=c(gt=3,lt=6)}.  If no op is specified, then
  \code{"$eq"} is assumed.  Additionally, the \code{"$oid"} operator can
  be used to specify that a value should be treated as a Mongo record
  identifier. 
  
  The \code{"$in"} and \code{"$nin"} are also ops, but the corrsponding
  value is a vector.  They test if the record is in or not in the
  specified value.  If the value is vector valued, and no operator is
  specified it defaults to \code{"$in"}.

  The function \code{buildJQuery} processes each of its arguments,
  adding them onto the query document.  The \code{rawfields} argument
  adds the fields onto the document without further processing.  It is
  useful for control arugments like \code{"$limit"} and \code{"$sort"}.
  
}
\value{
  The function \code{buildJQuery} returns a unicode string which
  contains the JSON query document.  The function \code{buildJQterm}
  returns a unicode string with just one field in the query document.
}
\references{
  The MongoDB 4.0 Manual:  \url{https://docs.mongodb.com/manual/}
  
}
\author{Russell Almond}
\seealso{
  \code{\link{as.json}}, \code{\link{parseMessage}},
  \code{\link{getOneRec}}, \code{\link{getManyRecs}}
  \code{\link[mongolite]{mongo}}

}
\examples{

## Low level test of the JQterm possibilities for fields.

stopifnot(buildJQterm("uid","Fred")=='"uid":"Fred"')
stopifnot(buildJQterm("uid",c("Phred","Fred"))=='"uid":{"$in":["Phred","Fred"]}')
time1 <- as.POSIXct("2018-08-16 19:12:19 EDT")
stopifnot(buildJQterm("time",time1)=='"time":{"$date":1534461139000}')
time1l <- as.POSIXlt("2018-08-16 19:12:19 EDT")
stopifnot(buildJQterm("time",time1l)=='"time":{"$date":1534461139000}')
time2 <- as.POSIXct("2018-08-16 19:13:19 EDT")
stopifnot(buildJQterm("time",c(time1,time2))==
          '"time":{"$in":[{"$date":1534461139000},{"$date":1534461199000}]}')
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
          '"time":{ "$gt":{"$date":1534461139000}, "$lt":{"$date":1534461199000} }')
stopifnot(buildJQterm("count",c(nin=1,2:4))==
          '"count":{"$nin":[1,2,3,4]}')
stopifnot(buildJQterm("count",c("in"=1,2:4))==
          '"count":{"$in":[1,2,3,4]}')
stopifnot(buildJQterm("count",c(ne=1,ne=5))==
          '"count":{ "$ne":1, "$ne":5 }')

## Some Examples of buildJQuery on complete queries.

stopifnot(buildJQuery(app="default",uid="Phred")==
          '{ "app":"default", "uid":"Phred" }')
stopifnot(buildJQuery("_id"=c(oid="123456789"))==
          '{ "_id":{ "$oid":"123456789" } }')
stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5))==
          '{ "name":"George", "count":{ "$gt":3, "$lt":5 } }')
stopifnot(buildJQuery(name="George",count=c(gt=3,lt=5),
                      rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))==
          '{ "name":"George", "count":{ "$gt":3, "$lt":5 }, "$limit":1, "$sort":{timestamp:-1} }')


}
\keyword{ interface }
\keyword{ database }
