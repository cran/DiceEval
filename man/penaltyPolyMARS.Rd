\name{penaltyPolyMARS}
\alias{penaltyPolyMARS}

\title{Choice of the penalty parameter for a PolyMARS model}
\description{
This function fit PolyMARS models with different values of the penalty parameter
and compute criteria.
}
\usage{penaltyPolyMARS(X,Y,test=NULL,graphic=FALSE)}

\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{test}{a data.frame containing the design and the response of a test set when available, the prediction criteria will be computed for the test data (default corresponds to no test set)}
  \item{graphic}{if TRUE the value of the criteria are represented}
}

\value{ A matrix which first line corresponds to the values of the penalty parameter
and the following to the R2-criteria evaluted on the learning set, by cross-validation
and on the test set \code{if test!=NULL}.
}
\seealso{\code{\link{modelFit}}, \code{\link{R2}} and \code{\link{crossValidation}}}
\note{The cross-validation is performed with 10 classes.}
\author{D. Dupuy}
\examples{
data(dataIRSN5D)
X	<- dataIRSN5D[,1:5]
Y	<- dataIRSN5D[,6]
data(testIRSN5D)
library(polspline)
Crit	<- penaltyPolyMARS(X,Y,test=testIRSN5D[,-7],graphic=TRUE)
}
\keyword{models}
