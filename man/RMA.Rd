\name{RMA}
\alias{RMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Relative Maximal Absolute Error}
\description{
Relative Maximal Absolute Error
}
\usage{
RMA(Y, Ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{a real vector with the values of the output}
  \item{Ypred}{a real vector with the predicted values at the same inputs}
}
\value{The \code{RMA} criterion represents the maximum of errors between exact values and predicted one:
\deqn{RMA = \max_{1\leq i\leq n} \frac{| Y \left( x_{i}\right)-\hat{Y} \left( x_{i}\right)|}
{\sigma_{Y}}}{RMA = max[(Y-Ypred)/sd(Y)]}
where \eqn{Y}{Y} is the output variable, \eqn{\hat{Y}}{Ypred} is the fitted model and
\eqn{\sigma_Y}{sd(Y)} denotes the standard deviation of \eqn{Y}{Y}.

The output of this function is a list with the following components: 
\item{max.value}{the value of the \code{RMA} criterion}
\item{max.data}{an integer \eqn{i} indicating the data \eqn{x^{i}} for which the \code{RMA} is reached}
\item{index}{a vector containing the data sorted according to the value of the errors}
\item{error}{a vector containing the corresponding value of the errors}
}
\author{D. Dupuy}
\seealso{other validation criteria as \code{\link{MAE}} or \code{\link{RMSE}}.}
\examples{
X     <- seq(-1,1,0.1)
Y     <- 3*X + rnorm(length(X),0,0.5)
Ypred <- 3*X
print(RMA(Y,Ypred))

# Illustration on Branin function
Branin <- function(x1,x2) {
   x1 <- x1*15-5   
   x2 <- x2*15
   (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
X <- matrix(runif(24),ncol=2,nrow=12)
Z <- Branin(X[,1],X[,2])
Y <- (Z-mean(Z))/sd(Z)

# Fitting of a Linear model on the data (X,Y)
modLm <- modelFit(X,Y,type = "Linear",formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))

# Prediction on a grid
u <- seq(0,1,0.1)
Y_test_real <- Branin(expand.grid(u,u)[,1],expand.grid(u,u)[,2])
Y_test_pred <- modelPredict(modLm,expand.grid(u,u))
Y_error <- matrix(abs(Y_test_pred-(Y_test_real-mean(Z))/sd(Z)),length(u),length(u))
contour(u, u, Y_error,45)
Y_pred <- modelPredict(modLm,X)
out <- RMA(Y,Y_pred)
for (i in 1:dim(X)[1]){
    points(X[out$index[i],1],X[out$index[i],2],pch=19,col='red',cex=out$error[i]*10)
}
}
\keyword{models}
