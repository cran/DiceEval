\name{R2}
\alias{R2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple R-Squared}
\description{
Coefficient of determination \eqn{R^{2}}{R2}
}
\usage{
R2(Y, Ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{a real vector with the values of the output}
  \item{Ypred}{a real vector with the predicted values at the same inputs}
}
\value{
\deqn{\code{R2}= 1 - \frac{SSE}{SST}}{R2=1-SSE/SST}
where \eqn{SSE= \sum_{i=1}^{n} (Y(x_{i}) - \hat{Y}(x_{i})^{2}}{SST= (Y(x1) - Ypred(x1) )^2+...+(Y(xn) - Ypred(xn) )^2} is the residual sum of squares 

and \eqn{SST= \sum_{i=1}^{n} (Y(x_{i}) - \bar{Y} )^{2}}{SST= (Y(x_{1}) - mean(Y) )^2+...+(Y(x_{n}) - mean(Y) )^2} is the total sum of squares. 

Note that the order of the input argument is important.
}
\author{D. Dupuy}
\examples{
X	<- seq(-1,1,0.1)
Y	<- 3*X + rnorm(length(X),0,0.5)
Ypred	<- 3*X
print(R2(Y,Ypred))
}
\keyword{models}
