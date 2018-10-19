\name{P4Message-class}
\Rdversion{1.1}
\docType{class}
\alias{P4Message-class}
\alias{app,P4Message-method}
\alias{context,P4Message-method}
\alias{details,P4Message-method}
\alias{timestamp,P4Message-method}
\alias{uid,P4Message-method}

\title{Class \code{"P4Message"}}
\description{
  
  A generic message sent in the Proc 4 system.  Contains information about the application associated with the message, the user, the context or task and message specific details.
  
  
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("P4Message", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Object of class \code{"character"} ~~ }
    \item{\code{app}:}{Object of class \code{"character"} ~~ }
    \item{\code{uid}:}{Object of class \code{"character"} ~~ }
    \item{\code{context}:}{Object of class \code{"character"} ~~ }
    \item{\code{sender}:}{Object of class \code{"character"} ~~ }
    \item{\code{mess}:}{Object of class \code{"character"} ~~ }
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} ~~ }
    \item{\code{data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{app}{\code{signature(x = "P4Message")}: ... }
    \item{context}{\code{signature(x = "P4Message")}: ... }
    \item{details}{\code{signature(x = "P4Message")}: ... }
    \item{timestamp}{\code{signature(x = "P4Message")}: ... }
    \item{uid}{\code{signature(x = "P4Message")}: ... }
	 }
}
\references{
  Almond, R. G., Steinberg, L. S., and Mislevy, R. J. (2002) 
 Enhancing the design and delivery of assessment systems: A four-process architecture \emph{Journal of Technology, Learning, and Assessment}, \bold{1}, (online), \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}

}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("P4Message")
}
\keyword{classes}
