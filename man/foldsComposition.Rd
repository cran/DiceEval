\name{foldsComposition}
\alias{foldsComposition}
\title{Setting up the Cross Validation}
\description{
  Randomly partitioning the data into folders
}
\usage{
foldsComposition(n, K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of observations}
  \item{K}{number of classes}
}
\seealso{\code{\link{crossValidation}}}
\value{a vector \code{v} of length \code{n} with \code{v[i]} = the class of observation
\code{i}.
}
\author{D. Dupuy}
\keyword{models}
\keyword{internal}
