\name{stepEvolution}
\alias{stepEvolution}
\title{Evolution of the stepwise model}
\description{
Graphical representation of the selected terms using stepwise
procedure for different values of the penalty parameter. This
function can also be used to select the penalty parameter.
}
\usage{
stepEvolution(X,Y,formula,P=1:7,test=NULL,graphic=TRUE)
}
\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{formula}{a formula for the initial model}
  \item{P}{a vector containing different values of the penalty
parameter for which a stepwise selected model is fitted.}
  \item{test}{where a test set is available, the prediction criteria are
	also evaluated on the test design (default corresponds to no data set)}
  \item{graphic}{if TRUE the value of the criteria are represented}
}
\value{a list which contains the different criteria for different values of the
penalty parameter.}
\seealso{\code{step} procedure for linear models.}
\note{Two plots are currently available :
\tabular{ll}{
a. \tab a tabular representing the selected terms for each values in \code{P}\cr
b. \tab the evolution of the \code{R2}, \code{Q2} and \code{R2test} if available
	for each stepwise-selected model\cr
}
}
\author{D. Dupuy}
\examples{
data(dataIRSN5D)
design <- dataIRSN5D[,1:5]
Y	   <- dataIRSN5D[,6]
out	   <- stepEvolution(design,Y,formulaLm(design,Y),P=c(1,2,5,10,20,30))
}
\keyword{models}
