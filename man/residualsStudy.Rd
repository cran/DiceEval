\name{residualsStudy}
\alias{residualsStudy}
\title{Plot residuals}
\description{
residualsStudy analyzes the residuals of a model: a plot of the residuals against the index, a plot
of the residuals against the fitted values, the representation of the
density and a normal Q-Q plot.
}
\usage{
residualsStudy(model)
}
\arguments{
  \item{model}{a fitted model obtained from \code{modelFit}}
}
\author{D. Dupuy}
\seealso{ \code{\link{modelFit}} and \code{\link{modelPredict}}}
\examples{
data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
library(gam)
modAm <- modelFit(X,Y,type = "Additive",formula=formulaAm(X,Y))
residualsStudy(modAm)
}
\keyword{models}
