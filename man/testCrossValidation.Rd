\name{testCrossValidation}
\alias{testCrossValidation}

\title{Test the robustess of the cross-validation procedure}
\description{
This function calculates the estimated K-fold cross-validation for different
values of \code{K}.
}
\usage{testCrossValidation(model,Kfold=c(2,5,10,20,30,40,dim(model$data$X)[1]),N=10)}

\arguments{
  \item{model}{a fitted model from \code{modelFit}}
  \item{Kfold}{a vector containing the values to test (default corresponds to 2,5,10,20,30,40 and the number of observations for leave-one-out procedure)}
  \item{N}{an integer given the number of times the K-fold cross-validation is performed for each value of K}
}

\value{a matrix of all the values obtained by K-fold cross-validation}
\note{For each value of K, the cross-validation procedure is repeated \code{N} times in
order to get an idea of the dispersion of the \code{Q2} criterion and of the \code{RMSE} by K-fold cross-validation.}
\author{D. Dupuy}

\seealso{\code{\link{crossValidation}}}
\examples{
\dontrun{
rm(list=ls())
# A 2D example
Branin <- function(x1,x2) {
  x1 <- x1*15-5   
  x2 <- x2*15
  (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# a 2D uniform design and the value of the response at these points
n <- 50
X <- matrix(runif(n*2),ncol=2,nrow=n)
Y <- Branin(X[,1],X[,2])

mod <- modelFit(X,Y,type="Linear",formula=formulaLm(X,Y))
out <- testCrossValidation(mod,N=20)
}
}
\keyword{models}


