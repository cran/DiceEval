\name{DiceEval-package}
\alias{DiceEval-package}
\alias{DiceEval}
\docType{package}
\title{Metamodels}
\description{
Construction and evaluation of metamodels.
\tabular{ll}{
Package: \tab DiceEval\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2014-05-27\cr
License: \tab GPL-3\cr
}
}
\details{
This package is dedicated to the construction of metamodels. A validation procedure is also proposed using usual criteria (RMSE, MAE etc.) and cross-validation procedure. Moreover, graphical tools help to choose the best value for the penalty parameter of a stepwise or a PolyMARS model. Another routine is dedicated to the comparison of metamodels.
}
\note{This work was conducted within the frame of the DICE (Deep Inside Computer Experiments) Consortium between ARMINES, Renault, EDF, IRSN, ONERA and TOTAL S.A. (http://emse.dice.fr/).

Functions \code{\link[gam]{gam}}, \code{\link[mda]{mars}} and \code{\link[polspline]{polymars}} are required for the construction of metamodels. \code{\link[DiceKriging]{km}} provides Kriging models.


}
\author{
D. Dupuy & C. Helbert
}
\references{
Friedman J. (1991), Multivariate Adaptative Regression Splines (invited paper),
\emph{Annals of Statistics}, \bold{10}/1, 1-141.

Hastie T. and Tibshirani R. (1990), Generalized Additive Models, \emph{Chapman and Hall, London}. 

Hastie T., Tibshirani R. and Friedman J. (2001), The Elements of Statistical Learning : Data Mining, Inference and Prediction, \emph{Springer}.

Helbert C. and Dupuy D. (2007-09-26), Retour d'exp?riences sur m?tamod?les : partie th?orique, \emph{Livrable r?dig? dans le cadre du Consortium DICE}.

Kooperberg C., Bose S. and Stone C.J. (1997), Polychotomous Regression, \emph{Journal of the American Statistical Association}, \bold{92} Issue 437, 117-127.

Rasmussen C.E. and Williams C.K.I. (2006), Gaussian Processes for Machine Learning, \emph{the MIT Press}, www.GaussianProcess.org/gpml.

Stones C., Hansen M.H., Kooperberg C. and Truong Y.K. (1997), Polynomial Splines and their Tensor Products in Extended Linear Modeling, \emph{Annals of Statistics}, \bold{25}/4, 1371-1470.}

\keyword{ package }
\seealso{\code{\link{modelFit}}, \code{\link{modelPredict}},
\code{\link{crossValidation}} and \code{\link{modelComparison}}

Different space-filling designs can be found in the \code{DiceDesign} package and we refer to the \code{DiceKriging} package for the construction of kriging models. This package takes part of a toolbox inplemented during the Dice consortium.}

\examples{
\dontrun{
rm(list=ls())
# A 2D example
Branin	<- function(x1,x2) {
x1	<- 1/2*(15*x1+5)   
x2	<- 15/2*(x2+1)
(x2 - 5.1/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# A 2D uniform design with n points in [-1,1]^2
n	<- 50
X	<- matrix(runif(n*2,-1,1),ncol=2,nrow=n)
Y	<- Branin(X[,1],X[,2])
Z	<- (Y-mean(Y))/sd(Y)

# Construction of a PolyMARS model with a penalty parameter equal to 2
library(polspline)
modPolyMARS	<- modelFit(X,Z,type = "PolyMARS",gcv=2.2)

# Prediction and comparison between the exact function and the predicted one
xtest	<- seq(-1, 1, length= 21) 
ytest	<- seq(-1, 1, length= 21)
Zreal	<- outer(xtest, ytest, Branin)
Zreal	<- (Zreal-mean(Y))/sd(Y)
Zpredict	<- modelPredict(modPolyMARS,expand.grid(xtest,ytest))
m	<- min(floor(Zreal),floor(Zpredict))
M	<- max(ceiling(Zreal),ceiling(Zpredict))
persp(xtest, ytest, Zreal, theta = 30, phi = 30, expand = 0.5,
	col = "lightblue",main="Branin function",zlim=c(m,M),
	ticktype = "detailed")

persp(xtest, ytest, matrix(Zpredict,nrow=length(xtest),
	ncol=length(ytest)), theta = 30, phi = 30, expand = 0.5,
	col = "lightblue",main="PolyMARS Model",zlab="Ypredict",zlim=c(m,M),
	ticktype = "detailed")

# Comparison of models
modelComparison(X,Y,type=c("Linear", "StepLinear","PolyMARS","Kriging"),
	formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2),penalty=log(dim(X)[1]), gcv=4)

# see also the demonstration example in dimension 5 (source: IRSN)
demo(IRSN5D)
}
}
\keyword{models}