stepEvolution <- function(X,Y,formula,P=1:7,K=10,test=NULL,graphic=TRUE){
	if(!is.numeric(Y)){
		stop("The response must be of class \'numeric\'.")
	}
	
	n <- length(Y)
	X <- as.data.frame(X)
	f <- dim(X)[2]
	
	modTmp <- modelFit(X,Y,type="Linear", formula=formula)
	fmlaPred <-  attr(modTmp$model$terms,"term.labels")
	nTerms <- length(fmlaPred)
	
	if(nTerms>n){
		stop("There are too many terms into the full length model")
	}
	R2criterion <- vector('numeric',length=length(P))
	Q2criterion <- vector('numeric',length=length(P))
	RMSEcriterion <- vector('numeric',length=length(P))
	ModelSize	<- vector('numeric',length=length(P))
	
	if(!is.null(test)){
	  test <- data.frame(test)
	  if (dim(test)[2]!=(f+1)){
	    test <- NULL
	  }
	  R2testcriterion <- vector('numeric',length=length(P))
	}

	m	 <- length(P)

	if(graphic==TRUE){
	  plot(NA, xlim=c(0,1), ylim=c(0,1), axes = FALSE,xlab='',ylab='',main="Selected variables during stepwise procedure",cex=1.3)
	  for (j in 0:m) {
		for (i in 0:nTerms) {
			polygon( c(j/(m+1), j/(m+1), (j+1)/(m+1), (j+1)/(m+1)),
		 		c(i/(nTerms+1), (i+1)/(nTerms+1), (i+1)/(nTerms+1), i/(nTerms+1)))
			if (i==0 & j!=0) text((j+1/2)/(m+1),(nTerms+1/2)/(nTerms+1),P[j],cex=0.7)
			if (j==0 & i!=0) text((1/2)/(m+1),(nTerms-i+1/2)/(nTerms+1),fmlaPred[i],cex=0.7)
		}
	  }
	} 

	for (p in 1:length(P)){
		smodel <- modelFit(X,Y,type="StepLinear", penalty = P[p],formula=formula)
  		variables	<- attr(smodel$model$terms,"term.labels")
		if (graphic == TRUE & length(variables) != 0){
  		   for (i in 1:length(variables)){
				r <- p
	   			s <- 1:nTerms
	  			J <- (fmlaPred==variables[i])
				polygon(c(r/(m+1),r/(m+1),(r+1)/(m+1),(r+1)/(m+1)),c((nTerms-s[J])/(nTerms+1),
					(nTerms-s[J]+1)/(nTerms+1),(nTerms-s[J]+1)/(nTerms+1),(nTerms-s[J])/(nTerms+1)),
					col = 'blue')
  		   }
		}
  		R2criterion[p]	<- R2(Y,smodel$model$fitted.values)
		tmp <- crossValidation(smodel,K=K)
		Q2criterion[p]	<- tmp$Q2
		RMSEcriterion[p] <- tmp$RMSE_CV
		ModelSize[p]	<- smodel$model$rank-1
		if(!is.null(test)){
		  Xtest	<- test[,1:f]
		  Ytest	<- test[,f+1]
		  YStep	<- modelPredict(smodel,newdata=Xtest)
		  R2testcriterion[p]	<- R2(Ytest,YStep)
		}
	}

	if(graphic==TRUE){
		if(is.null(test)){
		  m = min(R2criterion,Q2criterion); M=max(R2criterion,Q2criterion);
		  op <- par(ask=TRUE,mfrow = c(2, 2), oma=c(0,0,3,0))
		  plot(P,R2criterion,type='p',pch=19,xlab='Penalty parameter',ylab='R2',ylim=c(m,M))
		  plot(P,Q2criterion,type='p',pch=17,col='green3',xlab='Penalty parameter',ylab='Q2 (K=10)', ylim=c(m,M))
		  plot(P,R2criterion,type='p',pch=19,xlab='Penalty parameter',ylab='',ylim=c(m,M))
		  lines(P,Q2criterion,type='p',pch=17,col='green3')
		  legend("bottomleft",legend=c("R2","Q2 (K=10)"),lty=0,pch=c(19,17),col=c("black","green3"),cex=0.7)
		  plot(P,ModelSize,type='p',pch=19,col='violetred2',xlab='Penalty parameter',ylab='size of the fitted model')
		  par(op)
		  mtext("Stepwise: influence of the penalty parameter", side=3, line=0, font=1, cex=1.3)
		} else {
		  op <- par(ask=TRUE,mfrow = c(2, 2), oma=c(0,0,3,0))
		  m = min(R2criterion,Q2criterion,R2testcriterion)
		  M=max(R2criterion,Q2criterion,R2testcriterion)
		  plot(P,R2criterion,type='p',pch=19,xlab='Penalty parameter',ylab='R2',ylim=c(m,M))
		  plot(P,Q2criterion,type='p',pch=17,col='green3',xlab='Penalty parameter',ylab='Q2 (K=10)', ylim=c(m,M))
		  plot(P,R2testcriterion,type='p',pch=19,col='blue',xlab='Penalty parameter', ylab='R2test', ylim=c(m,M))
		  plot(P,ModelSize,type='p',pch=19,col='violetred2',xlab='Penalty parameter',ylab='size of the fitted model')
		  par(op)
		  mtext("Stepwise: influence of the penalty parameter", side=3, line=0, font=1, cex=1.3)
		} 
	}
	plot(P,RMSEcriterion,type='b',pch=19,xlab='Penalty parameter',ylab='RMSE CV',main=paste("RMSE criterion for K=",K,"folds cross-validation"))

	if(is.null(test)){
	  return(list(penalty=P,m=ModelSize,R2=R2criterion,Q2=Q2criterion,RMSE_CV=RMSEcriterion))
	} else return(list(penalty=P,m=ModelSize,R2=R2criterion,Q2=Q2criterion,R2test=R2testcriterion,RMSE_CV=RMSEcriterion))
}