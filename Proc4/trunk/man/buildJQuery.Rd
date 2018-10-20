\name{buildJQuery}
\alias{buildJQuery}
\alias{buildJQterm}
\title{Builds a JSON (Mongo DB) query}
\description{

  A JSON query is a list of name--value pairs.  This function turns its
  argument, which should consist of named values, into a JSON query
  string.  Comparison arguments can be added using names of the value
  part of the argument.  See details for more descriptions.

}
\usage{
buildJQuery(..., rawfields = character())
buildJQterm(name,value)
}
\arguments{
  \item{\dots}{
    A named list consisting of a collection of field names and values.
  }
  \item{rawfields}{
    Additional strings which are added to the end of the query, to
    enable hand coding of terms which don't fit into the model.
  }
  \item{name}{
    A character scalar giving the name of a JSON field.
  }
  \item{value}{
    The value for the field in the query.  Note that if names are
    supplied for the value, they should be JSON query comparison
    operators.
  }
}
\details{
 
  A JSON query is a collection of name-value pairs which looks something
  like: 
  \preformatted{{
      "name":"George",
      "count":{ "$gt":3, "$lt":5 },
      "context":{"$in":["normal","recovery"]}
  }}
  In this structure, the name of the field is followed by a colon and a
  value.  This will match documents in the database in which that field
  has that value.  Alternatively, a more complex expression can be given
  which use the comparison query operators.

  The \code{\dots} argument to \code{buildJQuery} should consist of
  named values.  The \var{name} is used as the name of the JSON field and the
  value is used as the value.  If the \var{value} is a vector of
  possible values.  \code{names(\var{value})} are interpreted as 
  query operators.  The function \code{buildJQterm} does most of the
  lifting of calculating the sub-expressions used to calculate the final
  query. 

  The following are the names of the operators currently supported.
  \describe{
    \item{No names}{If the \var{value} has length 1, this is interpreted
    as a single value.  If it has length greater than 1, it is interpreted
    as an \sQuote{in} query.}
    \item{\sQuote{eq},\sQuote{ne},\sQuote{gt},\sQuote{gte},\sQuote{lt},\sQuote{lte}}{These are the equals, not equals, greater than, greater than or
    equals, less than, and less than or equals operators.  Multiples of
    these can be combined to form a conjunctive query.  Note that
    multiple \sQuote{ne} terms work, but it is probably better to use
    \sQuote{nin}. }
    \item{\sQuote{in},\sQuote{nin}}{These check for whether the field
    matches any (\sQuote{in}) or none (\sQuote{nin}) of the values.  If
    \code{names(\var{value})[1]} is \sQuote{in} or \sQuote{nin}, then
    the other names are ignored.  Note that \sQuote{in} needs to be
    quoted in \R to be used as a name, however, in most cases it is
    easier to just leave the value blank and use the implicit naming.}
    \item{\sQuote{}}{This is a legal value mainly to support leaving the
    other names blank after \sQuote{in} or \sQuote{nin}.}
    \item{\sQuote{oid}}{This is a special operator that marks a mongo
    database ID.}
    }

    After the arguments to \code{buildJQuery} are processed by
    \code{buildJQterm}, they are concatenated together and surrounded by
    braces.  The \code{rawfields} allows other preformated JSON strings
    to be inserted between the braces.  This provides a limited
    extension capability.  If this is insufficient, (for example
    \sQuote{or} queries), it should be possible to build more complex
    queries using \code{buildJQterm}.
}
\value{
  For \code{buildJQquery}, a character scalar giving the JSON query.
  For \code{buildJQterm}, a character scalar giving one term of the JSON
  query. 
}
\references{
  \url{https://docs.mongodb.com/manual/tutorial/query-documents/}
}
\author{Russell Almond}
\note{
  The \pkg{jsonlite} \code{\link[jsonlite]{unbox}} command does not
  handle objects of type POSIXt correctly.  I've overrided with a
  private version which fixes that.  (This is a non-exported object.)
}
\seealso{
  \code{\link[jsonlite]{toJSON}} does much of the work behind the
  scenes.
  
}
\examples{

cat(buildJQterm("uid","Fred"),"\n")

cat(buildJQuery("uid"="Fred"),"\n")
cat(buildJQuery("uid"=c("Phred","Fred")),"\n")
cat(buildJQuery("time"=time1),"\n")
cat(buildJQuery("time"=time1l),"\n")
cat(buildJQuery("time"=c(time1,time2)),"\n")
cat(buildJQuery("time"=c(gt=time1)),"\n")
cat(buildJQuery("time"=c(lt=time1)),"\n")
cat(buildJQuery("time"=c(gte=time1)),"\n")
cat(buildJQuery("time"=c(lte=time1)),"\n")
cat(buildJQuery("time"=c(ne=time1)),"\n")
cat(buildJQuery("time"=c(eq=time1)),"\n")
cat(buildJQuery("time"=c(gt=time1,lt=time2)),"\n")
cat(buildJQuery("count"=c(nin=1,2:4)),"\n")
cat(buildJQuery("count"=c("in"=1,2:4)),"\n")
cat(buildJQuery("count"=c(ne=1,ne=5)),"\n")

cat(buildJQuery(app="default",uid="Phred"),"\n")
## oid is a special operator which indicates a mongo ID
cat(buildJQuery("_id"=c(oid="123456789")),"\n")
cat(buildJQuery(name="George",count=c(gt=3,lt=5)),"\n")
cat(buildJQuery(name="George",count=c(gt=3,lt=5),
                      rawfields=c('"$limit":1','"$sort":{timestamp:-1}')),"\n")


}
\keyword{ IO }
\keyword{ interfaces }
\keyword{ database }
