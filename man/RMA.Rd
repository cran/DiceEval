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
\value{
 a real which represents the maximum of errors between exact values and predicted ones:
\deqn{RMA = \max_{1\leq i\leq n} \frac{| Y \left( x_{i}\right)-\hat{Y} \left( x_{i}\right)|}
{\sigma_{Y}}}{RMA = max[(Y-Ypred)/sd(Y)]}
where \eqn{Y}{Y} is the output variable, \eqn{\hat{Y}}{Ypred} is the fitted model and
\eqn{\sigma_Y}{sd(Y)} denotes the standard deviation of \eqn{Y}{Y}.
}
\author{D. Dupuy}
\seealso{other validation criteria as \code{\link{MAE}} or \code{\link{RMSE}}.}
\examples{
X     <- seq(-1,1,0.1)
Y     <- 3*X + rnorm(length(X),0,0.5)
Ypred <- 3*X
print(RMA(Y,Ypred))
}
\keyword{models}
