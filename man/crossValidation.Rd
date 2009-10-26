\name{crossValidation}
\alias{crossValidation}
\title{K-fold Cross Validation}
\description{This function calculates the predicted values at each point of the design and gives an estimation of the R2 criterion using K-fold cross-validation.}
\usage{crossValidation(model, K)}
\arguments{
  \item{model}{an output of \code{modelFit} or a Kriging model fitted from \code{\link[DiceKriging]{km}}. This argument is the initial fitted model using all the data.}
  \item{K}{the number of groups into which the data should be split to apply cross-validation}
}
\value{
A list with the following components: 
\item{Ypred}{a vector of predicted values obtained using K-fold cross-validation
at the points of the design}
\item{Q2}{a real which is the estimation of the criterion \code{R2} obtained by
cross-validation}
In the case of a Kriging model, other component to the robustess of the procedure are proposed: 
\item{theta}{the range parameter theta estimated for each fold,}
\item{trend}{the trend parameter estimated for each fold,}
\item{shape}{the estimated shape parameter if the covariance structure is of type \code{powerexp}.}
}
\seealso{\code{\link{R2}}, \code{\link{modelFit}}, \code{\link{foldsComposition}}, \code{\link{testCrossValidation}}}
\note{When \code{K} is equal to the number of observations, \emph{leave-one-out} cross-validation
is performed.}
\author{D. Dupuy}
\examples{
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

# Linear model
modLm <- modelFit(X,Y,type = "Linear",formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
R2(Y,modLm$model$fitted.values)
crossValidation(modLm,K=10)$Q2

# a 2D uniform design and the value of the response at these points
n <- 16
X <- data.frame(x1=runif(n),x2=runif(n))
Y <- Branin(X[,1],X[,2])

# kriging model 1 : gaussian covariance structure, no trend, no nugget effect
library(DiceKriging)
model <- km(~1, design=X, response=Y, covtype="powexp")
K <- 10
out   <- crossValidation(model, K)
par(mfrow=c(2,2))
plot(c(0,1:K),c(model@covariance@range.val[1],out$theta[,1]),
	xlab='',ylab='Theta1')
plot(c(0,1:K),c(model@covariance@range.val[2],out$theta[,2]),
	xlab='',ylab='Theta2')
plot(c(0,1:K),c(model@covariance@shape.val[1],out$shape[,1]),
	xlab='',ylab='p1',ylim=c(0,2))
plot(c(0,1:K),c(model@covariance@shape.val[2],out$shape[,2]),
	xlab='',ylab='p2',ylim=c(0,2))
par(mfrow=c(1,1))}
\keyword{models}