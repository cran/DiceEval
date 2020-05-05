\name{modelComparison}
\alias{modelComparison}
\title{Comparison of different types of metamodels}
\description{
modelComparison fits different metamodels and returns \code{R2} and \code{RMSE} criteria relating to each.}
\usage{
modelComparison(X,Y, type="all",K=10,test=NULL,...)
}
\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{type}{a vector containing the type of models to compare. 
  
The default value is \code{"all"=c("Linear", "StepLinear","Additive", "PolyMARS", "MARS","Kriging")}}
	\item{K}{the number of folds for cross-validation (default value is set at 10)}
  \item{test}{a data.frame containing the design and the response of a test set when available, the prediction criteria will be evaluated on the test design (default corresponds to no test set)}
  \item{\dots}{according to the \code{type} argument, parameters can be specified (for example, \code{formula} and \code{penalty} for a stepwise procedure)}
}
\value{ A list containing two fields if the argument \code{test} equal \code{NULL} and three fields otherwise :
\item{Learning}{\code{R2} and \code{RMSE} criteria evaluated from learning set,}
\item{CV}{\code{Q2} and \code{RMSE_CV} criteria using K-fold cross-validation,}
\item{Test}{\code{R2} and \code{RMSE} criteria on the test set.}
A graphical tool to compare the value of the criteria is proposed.
}
\seealso{\code{\link{modelFit}} and \code{\link{crossValidation}}}
\author{D. Dupuy}
\examples{
\dontrun{
data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
data(testIRSN5D)
library(gam)
library(mda)
library(polspline)
crit  <- modelComparison(X,Y, type="all",test=testIRSN5D)

crit2 <- modelComparison(X,Y, type=rep("StepLinear",5),test=testIRSN5D,
		penalty=c(1,2,5,10,20),formula=Y~.^2)
    }
}
\keyword{models}
