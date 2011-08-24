\name{crossValidation}
\alias{crossValidation}
\title{K-fold Cross Validation}
\description{This function calculates the predicted values at each point of the design and gives an estimation of criterion using K-fold cross-validation.}
\usage{crossValidation(model, K)}
\arguments{
  \item{model}{an output of the \code{modelFit} function. This argument is the initial model fitted with all the data.}
  \item{K}{the number of groups into which the data should be split to apply cross-validation}
}
\value{
A list with the following components: 
\item{Ypred}{a vector of predicted values obtained using K-fold cross-validation at the points of the design}
\item{Q2}{a real which is the estimation of the criterion \code{R2} obtained by cross-validation}
\item{folds}{a list which indicates the partitioning of the data into the folds}
\item{RMSE_CV}{\code{\link{RMSE}} by K-fold cross-validation (see more details below)}
\item{MAE_CV}{\code{\link{MAE}}  by K-fold cross-validation (see more details below)}
In the case of a Kriging model, other components to test the robustess of the procedure are proposed: 
\item{theta}{the range parameter theta estimated for each fold,}
\item{trend}{the trend parameter estimated for each fold,}
\item{shape}{the estimated shape parameter if the covariance structure is of type \code{powerexp}.}

The principle of cross-validation is to split the data into \eqn{K} folds of approximately equal size \eqn{A_{1}{A1}, ..., A_{K}{AK}}. For \eqn{k=1} to \eqn{K}, a model \eqn{\hat{Y}^{(-k)}}{Y^(-k)} is fitted from the data \eqn{\cup_{j \neq k} A_{k}}{A1 U ... U AK} and this model is validated on the fold \eqn{A_{k}}{Ak}. Given a criterion of quality \eqn{L} (here, \eqn{L} could be the \code{RMSE} or the \code{MAE} criterion), the "evaluation" of the model consists in computing :
\deqn{L_{k} = \frac{1}{n/K} \sum_{i \in A_{k}} L \left( y_{i}, Y^{(-k)} (x_{i} )\right).}{Lk = 1/(n/K) Sum (i in Ak) L (yi,Y^(-k)(xi).}
The cross-validation criterion is the mean of the \eqn{K} criterion: \eqn{L}_CV\eqn{=\frac{1}{K} \sum_{k=1}^{K} L_{k}.}{1/K (L1+...+LK).}

The \code{Q2} criterion is defined as: \code{Q2}\eqn{=\code{R2}(\code{Y},\code{Ypred})} with \code{Y} the response value and \code{Ypred} the value fit by cross-validation.
}
\seealso{\code{\link{R2}}, \code{\link{modelFit}}, \code{\link{MAE}}, \code{\link{RMSE}}, \code{\link{foldsComposition}}, \code{\link{testCrossValidation}}}
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
mKm <- modelFit(X,Y,type="Kriging",formula=~1, covtype="powexp")
K <- 10
out   <- crossValidation(mKm, K)
par(mfrow=c(2,2))
plot(c(0,1:K),c(mKm$model@covariance@range.val[1],out$theta[,1]),
 	xlab='',ylab='Theta1')
 plot(c(0,1:K),c(mKm$model@covariance@range.val[2],out$theta[,2]),
 	xlab='',ylab='Theta2')
 plot(c(0,1:K),c(mKm$model@covariance@shape.val[1],out$shape[,1]),
 	xlab='',ylab='p1',ylim=c(0,2))
 plot(c(0,1:K),c(mKm$model@covariance@shape.val[2],out$shape[,2]),
 	xlab='',ylab='p2',ylim=c(0,2))
par(mfrow=c(1,1))}
\keyword{models}