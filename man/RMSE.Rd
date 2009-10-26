\name{RMSE}
\alias{RMSE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Root Mean Squared Error}
\description{
The root of the Mean Squared Error between the exact value and the predicted one.
}
\usage{
RMSE(Y, Ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{a real vector with the values of the output}
  \item{Ypred}{a real vector with the predicted values}
}
\value{
a real which represents the root of the mean squared error between the target response \eqn{Y}{Y}
and the fitted one \eqn{\hat{Y}}{Ypred}:
\deqn{RMSE = \sqrt{\frac{1}{n} \sum_{i=1}^{n} \left( Y \left( x_{i}\right)-\hat{Y}
		\left( x_{i}\right)\right)^2}.}{RMSE = sqrt(mean((Y-Ypred)^2))
.}
}

\author{D. Dupuy}
\seealso{other validation criteria as \code{\link{MAE}} or \code{\link{RMA}}}
\examples{
X    <- seq(-1,1,0.1)
Y    <- 3*X + rnorm(length(X),0,0.5)
Ypred <- 3*X
print(RMSE(Y,Ypred))
}
\keyword{models}
