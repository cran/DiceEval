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
print(modLm$model$coefficient)

\dontrun{
# illustration on a 2-dimensional example
Branin	<- function(x1,x2) {
x1	<- 1/2*(15*x1+5)   
x2	<- 15/2*(x2+1)
(x2 - 5.1/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# A 2D uniform design with 20 points in [-1,1]^2
n	<- 20
X	<- matrix(runif(n*2,-1,1),ncol=2,nrow=n)
Y	<- Branin(X[,1],X[,2])
Z	<- (Y-mean(Y))/sd(Y)

# Construction of a Kriging model
mKm	<- modelFit(X,Z,type = "Kriging")

# Prediction and comparison between the exact function and the predicted one
xtest	<- seq(-1, 1, length= 21) 
ytest	<- seq(-1, 1, length= 21)
Zreal	<- outer(xtest, ytest, Branin)
Zreal	<- (Zreal-mean(Y))/sd(Y)
Zpredict	<- modelPredict(mKm,expand.grid(xtest,ytest))

z <- abs(Zreal-matrix(Zpredict,nrow=length(xtest),ncol=length(ytest)))
contour(xtest, xtest, z,30)
points(X,pch=19)
}
}
\keyword{models}
