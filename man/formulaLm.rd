\name{formulaLm}
\alias{formulaLm}

\title{Construction of a formula Y ~ X1+...+Xp}
\description{
This function constructs a formula containing only the principal factors. 
}
\usage{formulaLm(X,Y)}

\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the associated response}
}

\value{an object of class \code{formula}.}
\note{The names of input variables are used to build the appropriate formula.}
\author{D. Dupuy}

\keyword{models}
\keyword{regression}
\keyword{internal}


