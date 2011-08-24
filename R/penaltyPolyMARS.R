penaltyPolyMARS <- function(X,Y,test=NULL,graphic=FALSE,K=10,Penalty = seq(0, 5, by = 0.2)){
	
	f		<- dim(X)[2]

	if(!is.null(test)){
	  test <- data.frame(test)
	  if (dim(test)[2]!=(f+1)){
	    test <- NULL
	  }
	  out		<- matrix(0,ncol=length(Penalty),nrow=6)
	} else out	<- matrix(0,ncol=length(Penalty),nrow=5)

	out[1,]	<- Penalty
	for (i in 1:length(Penalty)){
		modPolyMARS <- modelFit(X,Y,type = "PolyMARS",gcv=Penalty[i])
		out[2,i] 	<- R2(Y,modPolyMARS$model$fitted)
		out[3,i]	<- modPolyMARS$model$model.size-1
		CV_results  <- crossValidation(modPolyMARS,K=K)
		out[4,i]    <- CV_results$Q2
		out[5,i]    <- CV_results$RMSE_CV
		if (!is.null(test)){
			Xtest <- test[,1:f]
			Ytest	<- test[,f+1]
			YPm <- modelPredict(model=modPolyMARS,newdata=Xtest)
			out[6,i] <- R2(Ytest,YPm)
		}
	}

	if(graphic==TRUE){
		if(is.null(test)){
		  m = min(out[c(2,4),]); M=max(out[c(2,4),]);
		  op <- par(ask=TRUE,mfrow = c(2, 2), oma=c(0,0,3,0))
		  plot(out[1,],out[2,],type='b',pch=19,xlab='Penalty parameter',ylab='R2',ylim=c(m,M))
		  plot(out[1,],out[4,],type='b',pch=17,col='green3',xlab='Penalty parameter',ylab=paste("Q2 (K=",K,")",sep=""),ylim=c(m,M))
		  plot(out[1,],out[2,],type='b',pch=19,xlab='Penalty parameter',ylab='',ylim=c(m,M))
		  lines(out[1,],out[4,],type='b',pch=17,col='green3')
		  legend("bottomleft",legend=c("R2","Q2 (K=10)"),lty=0,pch=c(19,17),col=c("black","green3"),cex=0.7)
		  plot(out[1,],out[3,],type='b',pch=19,col='violetred2',xlab='Penalty parameter',ylab='size of the fitted model')
		  par(op)
		  mtext("PolyMARS: influence of the penalty parameter", side=3, line=0, font=1, cex=1.3)
		} else {
		  op <- par(ask=TRUE,mfrow = c(2, 2), oma=c(0,0,3,0))
		  m = min(out[c(2,4,6),]); M=max(out[c(2,4,5),]);
		  plot(out[1,],out[2,],type='b',pch=19,xlab='Penalty parameter',ylab='R2',ylim=c(m,M))
		  plot(out[1,],out[4,],type='b',pch=17,col='green3',xlab='Penalty parameter',ylab=paste("Q2 (K=",K,")",sep=""),ylim=c(m,M))
		  plot(out[1,],out[6,],type='b',pch=19,col='blue',xlab='Penalty parameter',ylab='R2test',ylim=c(m,M))
		  plot(out[1,],out[3,],type='b',pch=19,col='violetred2',xlab='Penalty parameter',ylab='size of the fitted model')
		  par(op)
		  mtext("PolyMARS: influence of the penalty parameter", side=3, line=0, font=1, cex=1.3)
		}
		par(ask=TRUE)
		plot(out[1,],out[5,],type='b',pch=19,xlab='Penalty parameter',ylab='RMSE ',ylim=c(min(out[5,]),max(out[5,])))
		par(ask=FALSE)

	}

	out <- data.frame(out)
	if(is.null(test)){
	  rownames(out) <- c("a","R2","m","Q2","RMSE CV")
	} else rownames(out) <- c("a","R2","m","Q2","R2test","RMSE CV")
	return(out)
}