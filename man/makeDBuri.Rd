\name{makeDBuri}
\alias{makeDBuri}
\title{Creates the URI needed to connect to a mongo database.}
\description{

  This function formats the universal record indicator (URI) for
  connecting to a Mongo database.  It is mostly a utility function for
  formatting the string.
  
}
\usage{
makeDBuri(username = "", password = "", host = "localhost", port = "")
}
\arguments{
  \item{username}{The name of the database user (login credential), or
    an empty string if no username is required.}
  \item{password}{The name of the database password (login credential), or
    an empty string if no password is required.}
  \item{host}{The name or IP address of the system hosting the database.}
  \item{port}{The port to be used for connections.  Note that the
    port for a default configuration of mongo is 27018.  This can be
    left blank to use the default port.}
}
\value{

  A character string giving the database URI which can be passed to the
  \code{\link[mongolite]{mongo}} function to create a database
  collection handle.

  Note that the password is stored in clear text, so appropriate care
  should be taken with the result of this function.

}
\author{Russell Almond}
\seealso{

  \code{\link{MongoDB}}, \code{\link[mongolite]{mongo}}

  This is an input argument to a number of other classes which use mongo
  connections.
  
}
\examples{

stopifnot(makeDBuri()=="mongodb://localhost")

stopifnot(makeDBuri(user="admin",password="secret")==
                    "mongodb://admin:secret@localhost")
## No password
stopifnot(makeDBuri(user="admin")==
                    "mongodb://admin@localhost")

stopifnot(makeDBuri(host="example.com",port=12345) ==
          "mongodb://example.com:12345")

}
\keyword{ interface }
\keyword{ database }
