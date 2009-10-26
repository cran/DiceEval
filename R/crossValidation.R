crossValidation <- function(model,K){

	if(class(model)=="km"){
		# Model fitted with all observations = model
		# design of experiments
	 	X <- data.frame(model@X)
	 	p <- dim(X)[2]
		n <- dim(X)[1]
		classe <- foldsComposition(n,K)	
		# input values at the points of X
	 	Y <- data.frame(model@y) 
	 	Ypred <- vector("numeric", length = n)
	 	theta <- matrix(0,nrow=K,ncol=p)
		trend <- matrix(0,nrow=K,ncol=length(model@trend.coef))
		if(model@covariance@param.n==2*p){
			shape <- matrix(0,nrow=K,ncol=p)
		}
		# Input parameters for the 'km' model
		formula <- model@trend.formula
		covtype <- model@covariance@name		
	    for (i in 1:K) {
			indice <- classe == i
			# fitting the 'km' model
			modi <- km(formula=model@trend.formula, design=model@X[!indice,], response=model@y[!indice,],
				covtype=model@covariance@name, nugget = model@covariance@nugget, 
				nugget.estim = model@covariance@nugget.estim, noise.var = model@noise.var, 
		 		penalty = model@penalty, optim.method = model@optim.method, 
		 		lower = model@lower, upper = model@upper, parinit = model@parinit,
		 		control = model@control, gr = model@gr)
				# ---- tous les modèles sont estimés avec la même initialisation (parinit ?)
				theta[i,] <- modi@covariance@range.val
				trend[i,] <- modi@trend.coef	
			if(model@covariance@param.n==2*p){
				shape[i,] <- modi@covariance@shape.val
			}
			# predicting at testdata points
			Ypred[indice] <- predict.km(modi, newdata = X,"UK")$mean[indice]
	    }
		if(model@covariance@param.n==2*p){
			out <- list(Q2 = 1 - mean((Y[,1]-Ypred)^2)/mean((Y[,1]-mean(Y))^2),theta=theta,shape=shape,trend=trend)
		} else out <- list(Q2 = 1 - mean((Y[,1]-Ypred)^2)/mean((Y[,1]-mean(Y))^2),theta=theta,trend=trend)
		return(out)
	} else {
		X <- model$data$X
		Y <- model$data$Y
	    p <- dim(X)[2]
		n <- dim(X)[1]
		classe <- foldsComposition(n,K)
		fmla=NA; degree <- NA; a <- NA; k <- NA	
	
		switch(model$type,
	        "Linear"     = { fmla <- model$formula},
			"Additive"   = { fmla <- model$formula},
			"StepLinear" = { fmla <- model$formula; k <- model$penalty},
			"PolyMARS"   = { a <- model$gcv},
			"MARS"       = { degree <- model$degree})
		Ypred  <- vector("numeric",length=n)
		for (i in 1:K){
		  indice 		<- classe==i
		  design 		<- X[!indice,]
		  Ycv			<- Y[!indice]
		  modi 			<- modelFit(design,Ycv,type=model$type,formula=fmla,degree=degree,gcv=a,penalty=k)
		  Ypred[indice] <- modelPredict(modi,newdata=X)[indice]
		}
		return(list(Ypred=Ypred,Q2=1 - mean((Y-Ypred)^2)/mean((Y-mean(Y))^2)))
	}
}

#-----------------------------------------------------------------#
#				FoldsComposition					#
#-----------------------------------------------------------------#

foldsComposition <- function(n,K){

	if (K==n){
		classe <- seq(1,n,by=1)
	} else {
	  nb_elem	<- rep(floor(n/K),K)
	  a_ajouter	<- n - floor(n/K)*K
	  if (a_ajouter>0){
	    nb_elem[1:a_ajouter]	<- nb_elem[1:a_ajouter]+1
	  }
	  if (sum(nb_elem)!=n) stop("Problem during the construction of the classes.")

	  classe <- vector("numeric",length=n)
	  i <- 1
	  while (i<=n){
		tmp <- floor(runif(1, min=1, max=K+1))
	    if (nb_elem[tmp]!=0){
		classe[i]	 <- tmp
		nb_elem[tmp] <- nb_elem[tmp]-1
		i 		 <- i+1
	    }
	  }
	}
	return(classe)
  }