\name{modelFit}
\alias{modelFit}
\title{Fitting metamodels}
\description{
	\code{modelFit} is used to fit a metamodel of class \code{lm}, \code{gam},
	\code{mars}, \code{polymars} or \code{km}.
}
\usage{
modelFit (X,Y, type, ...)
}
\arguments{
  \item{X}{a data.frame containing the design of experiments}
  \item{Y}{a vector containing the response variable}
  \item{type}{represents the method used to fit the model: 
	\tabular{ll}{
	\code{"Linear"} 	  \tab linear model, \cr
	\code{"StepLinear"} \tab stepwise, \cr
	\code{"Additive"}	  \tab gam, \cr
	\code{"MARS"} 	  \tab mars \cr
	\code{"PolyMARS"}	  \tab polymars \cr
	\code{"Kriging"}	\tab kriging model. \cr
	}}
  \item{\dots}{corresponds to the parameter(s) of the model. The list of the needed arguments for each type of models is given below:
	\tabular{ll}{
	\code{"Linear"} 	  \tab \code{formula} (see \code{\link{formulaLm}}), \cr
	\code{"StepLinear"} \tab \code{formula} \cr
				  \tab \code{penalty} parameter, \cr
	\code{"Additive"}	  \tab \code{formula} (see \code{\link{formulaAm}}), \cr
	\code{"MARS"} 	  \tab \code{degree}, \cr
	\code{"PolyMARS"}	  \tab \code{gcv} criteria. \cr
	\code{"Kriging"} \tab \code{formula} \cr
	\tab \code{covtype} \cr
	}}
}
\value{
A list with the following components: 
\item{X}{a data frame representing the design of experiments}
\item{Y}{a vector representing the response}
\item{type}{the type of metamodel}
\item{model}{a fitted model of the specified class} and the value of the parameter(s) depending on the fitted model.
}
\author{D. Dupuy}
\seealso{ \code{\link{modelPredict}}}
\examples{
# A 2D example
Branin <- function(x1,x2) {
  x1 <- x1*15-5   
  x2 <- x2*15
  (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# a 2D uniform design and the value of the response at these points
X <- matrix(runif(24),ncol=2,nrow=12)
Z <- Branin(X[,1],X[,2])
Y <- (Z-mean(Z))/sd(Z)

# construction of a linear model
modLm <- modelFit(X,Y,type = "Linear",formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
summary(modLm$model)

\dontrun{
# construction of a stepwise-selected model 
modStep <- modelFit(X,Y,type = "StepLinear",penalty=log(dim(X)[1]),
		formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
summary(modStep$model)

# construction of an additive model
library(gam)
modAm <- modelFit(X,Y,type = "Additive",formula=Y~s(X1)+s(X2))
summary(modAm$model)

# construction of a MARS model of degree 2
library(mda)
modMARS <- modelFit(X,Y,type = "MARS",degree=2)
print(modMARS$model)

# construction of a PolyMARS model with a penalty parameter equal to 1
library(polspline)
modPolyMARS <- modelFit(X,Y,type = "PolyMARS",gcv=1)
summary(modPolyMARS$model)

# construction of a Kriging model
modKm <- modelFit(X,Y,type = "Kriging")
str(modKm$model)
}
}
\keyword{models}