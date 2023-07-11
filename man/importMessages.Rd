\name{importMessages}
\alias{importMessages}
\alias{importMessages,MongoQueue-method}
\title{Imports a file full of messages into a message queue.}
\description{

  Interts the contents of a JSON file full of messages into the message queue.

}
\usage{
importMessages(queue, filelist, data.dir)
\S4method{importMessages}{MongoQueue}(queue, filelist, data.dir)
}
\arguments{
  \item{queue}{An object of class \code{\linkS4class{MessageQueue}} to be loaded.}
  \item{filelist}{A list of filenames of files containing data to be loaded.}
  \item{data.dir}{A character scalar giving the pathname of the directory containing the data files.}
}
\value{
  No particular return message.  Used for its side effects.
}
\author{Russell Almond}
\note{
  Current implementation uses the shell function `mongoimport` which may not
  be the best implementation if the Mongo server is on a different machine.
}
\seealso{
  \code{\linkS4class{MongoQueue}}, \code{\link{cleanMessageQueue}}
}
\examples{
  \dontrun{
    mq <- MongoQueue("Test",mongo::MongoDB("TestMessages","test"))
    importMessages(mq,c("PretestResults.json","TestResults.json"),"/usr/local/share/Proc4/data/")
  }
}
\keyword{ database }
\keyword{ interface }

