\name{formulaAm}
\alias{formulaAm}

\title{Construction of a formula Y~s(X1)+...+s(Xp)}
\description{
This function constructs a formula based on splines for additive models. 
}
\usage{formulaAm(X,Y)}

\arguments{
 \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the associated response}
}

\value{an object of class \code{formula}.}
\note{The names of input variables are used to build the appropriate formula.}
\author{D. Dupuy}

\examples{
data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
formulaAm(X,Y)
}


\keyword{models}
\keyword{regression}
\keyword{internal}

