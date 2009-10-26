\name{modelPredict}
\alias{modelPredict}
\title{Prediction at newdata for a fitted metamodel}
\description{
\code{modelPredict} computes predicted values based on the model given in argument.
}
\usage{
modelPredict(model,newdata)
}
\arguments{
  \item{model}{a fitted model obtained from \code{modelFit}}
  \item{newdata}{a matrix (or a data frame) which represents the predictor values at which the fitted values will be computed.}
}
\value{
a vector of predicted values, obtained by evaluating the model at \code{newdata}.
}
\author{D. Dupuy}
\seealso{\code{\link{modelFit}}}
\examples{
X  <- seq(-1,1,l=21)
Y  <- 3*X + rnorm(21,0,0.5)
# construction of a linear model
modLm <- modelFit(X,Y,type = "Linear",formula="Y~.")
newdata  <- seq(-1,1,length=21)
Ypred <- modelPredict(modLm,newdata)
}
\keyword{models}
