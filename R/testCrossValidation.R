testCrossValidation <- function(model,Kfold=c(2,5,10,20,30,40,dim(model$data$X)[1])){
	
	n <- dim(model$data$X)[1]

	out <- matrix(0,ncol=10,nrow=length(Kfold))	

	for(i in 1:length(Kfold)){
	  if(Kfold[i]==n){
		out[i,1:10] <- crossValidation(model,K=Kfold[i])$Q2
	  } else for (j in 1:10){
	    out[i,j] <- crossValidation(model,K=Kfold[i])$Q2
	  }
	}

	plot(rep(Kfold,each=10),as.numeric(t(out)),type='p',xlab='Number of folds',ylab='Q2',main='k-fold cross validation')
	
	return(out)
}