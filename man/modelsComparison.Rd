\name{modelsComparison}
\alias{modelsComparison}
\title{Comparison of different types of metamodels}
\description{
modelsComparison fits different metamodels and returns R2 and Q2 criteria relating to each.}
\usage{
modelsComparison(X,Y, type="all",test=NULL,...)
}
\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{type}{a vector containing the type of models to compare. 

The default value is \code{"all"=c("Linear", "StepLinear","Additive", "PolyMARS", "MARS")}}
  \item{test}{a data.frame containing the design and the response of a test set when available, the prediction criteria will be evaluated on the test design (default corresponds to no test set)}
  \item{\dots}{according to the \code{type} argument, parameters can be specified (for example, \code{formula} and \code{penalty} for a stepwise procedure)}
}
\value{ A list containing two fields if the arguments \code{test} is \code{NULL} and three fields otherwise :
\item{Learning}{R2 and RMSE criteria evaluated from learning set,}
\item{CV}{estimation of the R2 and RMSE criteria using K-fold cross-validation,}
\item{Test}{R2 and RMSE criteria on the test set.}
A graphical tool to compare the value of the criteria is proposed.
}
\seealso{\code{\link{modelFit}} and \code{\link{crossValidation}}}
\note{The cross-validation is performed with 10 classes.}
\author{D. Dupuy}
\examples{
data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
data(testIRSN5D)
library(gam)
library(mda)
library(polspline)
crit  <- modelsComparison(X,Y, type="all",test=testIRSN5D)
crit2 <- modelsComparison(X,Y, type=rep("StepLinear",5),test=testIRSN5D,
			penalty=c(1,2,5,10,20),formula=Y~.^2)
}
\keyword{models}
