\name{stepEvolution}
\alias{stepEvolution}
\title{Evolution of the stepwise model}
\description{
Graphical representation of the selected terms using stepwise
procedure for different values of the penalty parameter.
}
\usage{
stepEvolution(X,Y,formula,P=1:7,K=10,test=NULL,graphic=TRUE)
}
\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{formula}{a formula for the initial model}
  \item{P}{a vector containing different values of the penalty
parameter for which a stepwise selected model is fitted}
  \item{K}{the number of folds for the cross-validation procedure}
  \item{test}{an additional data set on which the prediction criteria are evaluated (default corresponds to no test data set)}
  \item{graphic}{if \code{TRUE} the values of the criteria are represented}
}
\value{a list with the different criteria for different values of the
penalty parameter. This list contains:
\item{penalty}{the values for the penalty parameter}
\item{m}{size \code{m} of the selected model for each value in \code{P}}
\item{R2}{the value of the \code{R2} criterion for each model}

According to the value of the \code{test} argument, other criteria are calculated:
\tabular{ll}{
a. \tab If a test set is available, \code{R2test} contains the value of the \code{R2} 
criterion on the test set  \cr
b. \tab If no test set is available, the \code{Q2} and the \code{RMSE} computed by
cross-validation are done.
}}

\seealso{\code{step} procedure for linear models.}
\note{Plots are also available. 
A tabular represents the selected terms for each value in \code{P}.

The evolution of the \code{R2} criterion, the evolution of the size \code{m} of the selected
model and criteria on the test set or by K-folds cross-validation are represented.

These graphical tools can be used to select the best value for the penalty parameter.
}
\author{D. Dupuy}
\examples{
\dontrun{
data(dataIRSN5D)
design <- dataIRSN5D[,1:5]
Y	   <- dataIRSN5D[,6]
out	   <- stepEvolution(design,Y,formulaLm(design,Y),P=c(1,2,5,10,20,30))
}
}
\keyword{models}
