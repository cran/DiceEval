pkgname <- "DiceEval"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('DiceEval')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("DiceEval-package")
### * DiceEval-package

flush(stderr()); flush(stdout())

### Name: DiceEval-package
### Title: Metamodels
### Aliases: DiceEval-package DiceEval
### Keywords: package models

### ** Examples

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



cleanEx()
nameEx("MAE")
### * MAE

flush(stderr()); flush(stdout())

### Name: MAE
### Title: Mean Absolute Error
### Aliases: MAE
### Keywords: models

### ** Examples

X	<- seq(-1,1,0.1)
Y	<- 3*X + rnorm(length(X),0,0.5)
Ypred	<- 3*X
MAE(Y,Ypred)



cleanEx()
nameEx("R2")
### * R2

flush(stderr()); flush(stdout())

### Name: R2
### Title: Multiple R-Squared
### Aliases: R2
### Keywords: models

### ** Examples

X	<- seq(-1,1,0.1)
Y	<- 3*X + rnorm(length(X),0,0.5)
Ypred	<- 3*X
print(R2(Y,Ypred))



cleanEx()
nameEx("RMA")
### * RMA

flush(stderr()); flush(stdout())

### Name: RMA
### Title: Relative Maximal Absolute Error
### Aliases: RMA
### Keywords: models

### ** Examples

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



cleanEx()
nameEx("RMSE")
### * RMSE

flush(stderr()); flush(stdout())

### Name: RMSE
### Title: Root Mean Squared Error
### Aliases: RMSE
### Keywords: models

### ** Examples

X    <- seq(-1,1,0.1)
Y    <- 3*X + rnorm(length(X),0,0.5)
Ypred <- 3*X
print(RMSE(Y,Ypred))



cleanEx()
nameEx("crossValidation")
### * crossValidation

flush(stderr()); flush(stdout())

### Name: crossValidation
### Title: K-fold Cross Validation
### Aliases: crossValidation
### Keywords: models

### ** Examples

rm(list=ls())
# A 2D example
Branin <- function(x1,x2) {
  x1 <- x1*15-5   
  x2 <- x2*15
  (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# a 2D uniform design and the value of the response at these points
n <- 50
X <- matrix(runif(n*2),ncol=2,nrow=n)
Y <- Branin(X[,1],X[,2])

# Linear model
modLm <- modelFit(X,Y,type = "Linear",formula=Y~X1+X2+X1:X2+I(X1^2)+I(X2^2))
R2(Y,modLm$model$fitted.values)
crossValidation(modLm,K=10)$Q2

# a 2D uniform design and the value of the response at these points
n <- 16
X <- data.frame(x1=runif(n),x2=runif(n))
Y <- Branin(X[,1],X[,2])

# kriging model 1 : gaussian covariance structure, no trend, no nugget effect
mKm <- modelFit(X,Y,type="Kriging",formula=~1, covtype="powexp")
K <- 10
out   <- crossValidation(mKm, K)
par(mfrow=c(2,2))
plot(c(0,1:K),c(mKm$model@covariance@range.val[1],out$theta[,1]),
 	xlab='',ylab='Theta1')
 plot(c(0,1:K),c(mKm$model@covariance@range.val[2],out$theta[,2]),
 	xlab='',ylab='Theta2')
 plot(c(0,1:K),c(mKm$model@covariance@shape.val[1],out$shape[,1]),
 	xlab='',ylab='p1',ylim=c(0,2))
 plot(c(0,1:K),c(mKm$model@covariance@shape.val[2],out$shape[,2]),
 	xlab='',ylab='p2',ylim=c(0,2))
par(mfrow=c(1,1))


graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("formulaAm")
### * formulaAm

flush(stderr()); flush(stdout())

### Name: formulaAm
### Title: Construction of a formula Y~s(X1)+...+s(Xp)
### Aliases: formulaAm
### Keywords: models regression internal

### ** Examples

data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
formulaAm(X,Y)



cleanEx()
nameEx("modelComparison")
### * modelComparison

flush(stderr()); flush(stdout())

### Name: modelComparison
### Title: Comparison of different types of metamodels
### Aliases: modelComparison
### Keywords: models

### ** Examples

data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
data(testIRSN5D)
library(gam)
library(mda)
library(polspline)
crit  <- modelComparison(X,Y, type="all",test=testIRSN5D)
crit2 <- modelComparison(X,Y, type=rep("StepLinear",5),test=testIRSN5D,
		penalty=c(1,2,5,10,20),formula=Y~.^2)



cleanEx()
nameEx("modelFit")
### * modelFit

flush(stderr()); flush(stdout())

### Name: modelFit
### Title: Fitting metamodels
### Aliases: modelFit
### Keywords: models

### ** Examples

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



cleanEx()
nameEx("modelPredict")
### * modelPredict

flush(stderr()); flush(stdout())

### Name: modelPredict
### Title: Prediction at newdata for a fitted metamodel
### Aliases: modelPredict
### Keywords: models

### ** Examples

X  <- seq(-1,1,l=21)
Y  <- 3*X + rnorm(21,0,0.5)
# construction of a linear model
modLm <- modelFit(X,Y,type = "Linear",formula="Y~.")
print(modLm$model$coefficient)

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



cleanEx()
nameEx("penaltyPolyMARS")
### * penaltyPolyMARS

flush(stderr()); flush(stdout())

### Name: penaltyPolyMARS
### Title: Choice of the penalty parameter for a PolyMARS model
### Aliases: penaltyPolyMARS
### Keywords: models

### ** Examples

data(dataIRSN5D)
X	<- dataIRSN5D[,1:5]
Y	<- dataIRSN5D[,6]
data(testIRSN5D)
library(polspline)
Crit	<- penaltyPolyMARS(X,Y,test=testIRSN5D[,-7],graphic=TRUE)



cleanEx()
nameEx("residualsStudy")
### * residualsStudy

flush(stderr()); flush(stdout())

### Name: residualsStudy
### Title: Plot residuals
### Aliases: residualsStudy
### Keywords: models

### ** Examples

data(dataIRSN5D)
X <- dataIRSN5D[,1:5]
Y <- dataIRSN5D[,6]
library(gam)
modAm <- modelFit(X,Y,type = "Additive",formula=formulaAm(X,Y))
residualsStudy(modAm)



cleanEx()
nameEx("stepEvolution")
### * stepEvolution

flush(stderr()); flush(stdout())

### Name: stepEvolution
### Title: Evolution of the stepwise model
### Aliases: stepEvolution
### Keywords: models

### ** Examples

data(dataIRSN5D)
design <- dataIRSN5D[,1:5]
Y	   <- dataIRSN5D[,6]
out	   <- stepEvolution(design,Y,formulaLm(design,Y),P=c(1,2,5,10,20,30))



cleanEx()
nameEx("testCrossValidation")
### * testCrossValidation

flush(stderr()); flush(stdout())

### Name: testCrossValidation
### Title: Test the robustess of the cross-validation procedure
### Aliases: testCrossValidation
### Keywords: models

### ** Examples

rm(list=ls())
# A 2D example
Branin <- function(x1,x2) {
  x1 <- x1*15-5   
  x2 <- x2*15
  (x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}
# a 2D uniform design and the value of the response at these points
n <- 50
X <- matrix(runif(n*2),ncol=2,nrow=n)
Y <- Branin(X[,1],X[,2])

mod <- modelFit(X,Y,type="Linear",formula=formulaLm(X,Y))
out <- testCrossValidation(mod,N=20)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
