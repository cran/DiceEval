\name{penaltyPolyMARS}
\alias{penaltyPolyMARS}

\title{Choice of the penalty parameter for a PolyMARS model}
\description{
This function fits a PolyMARS model for different values of the penalty parameter
and compute criteria.
}
\usage{penaltyPolyMARS(X,Y,test=NULL,graphic=FALSE,K=10,
		Penalty=seq(0,5,by=0.2))}

\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{test}{a data.frame containing the design and the response of a test set when available, the prediction criteria will be computed for the test data (default corresponds to no test set)}
  \item{graphic}{if TRUE the values of the criteria are represented}
  \item{K}{the number of folds for cross-validation (by default, K=10)}
  \item{Penalty}{a vector containing the values of the penalty parameter}
}

\value{ A matrix which first line corresponds to the values of the penalty parameter
and the following to the \code{R2} criteria evaluted on the learning set, by cross-validation
and on the test set \code{if test!=NULL}.

The \code{RMSE} computed by cross-validation is also provided. Note that the penalty parameter could be chosen by minimizing the value of the \code{RMSE} by cross-validation.
}
\seealso{\code{\link{modelFit}}, \code{\link{R2}} and \code{\link{crossValidation}}}
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
