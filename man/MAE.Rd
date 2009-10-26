\name{MAE}
\alias{MAE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mean Absolute Error}
\description{
  The mean of absolute errors between real values and predictions.
}
\usage{
MAE(Y, Ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{a real vector with the values of the output}
  \item{Ypred}{a real vector with the predicted values at the same inputs}
}
\value{
 a real which represents the mean of the absolute errors between the real and the predicted values:
\deqn{MAE = \frac{1}{n} \sum_{i=1}^{n} | Y \left( x_{i}\right)-\hat{Y} \left( x_{i}\right)|}{MAE = 1/n * [|Y(x1)-Ypred(x1)|+...+|Y(xn)-Ypred(xn)|]}
where \eqn{x_{i}}{xi} denotes the points of the experimental design, \eqn{Y} the output of the computer code and \eqn{\hat{Y}}{Ypred} the fitted model.
}
\author{D. Dupuy}
\seealso{other quality criteia as \code{\link{RMSE}} and \code{\link{RMA}}.}
\examples{
X	<- seq(-1,1,0.1)
Y	<- 3*X + rnorm(length(X),0,0.5)
Ypred	<- 3*X
MAE(Y,Ypred)
}
\keyword{models}
