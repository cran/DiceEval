modelsComparison <- function (X,Y,type="all",test = NULL,...) {

    if (length(type) == 1 && type == "all") {
        type <- c("Linear", "StepLinear","Additive", "PolyMARS", "MARS")
    }
	
    data <- data.frame(X, Y)
    X <- data.frame(X)
    f <- dim(X)[2]
    n <- dim(data)[1]
	
    # Learning criteria
	learningCrit 			<- data.frame(matrix(0, nrow = 2, ncol = length(type)))
    colnames(learningCrit) 	<- type
    rownames(learningCrit) <- c("R2", "RMSE")
	# Cross Validation criteria
	CVCrit <- data.frame(matrix(0, nrow = 2, ncol = length(type)))
    colnames(CVCrit) <- type
    rownames(CVCrit) <- c("Q2", "RMSE")
	# Test criteria
	testCrit <- data.frame(matrix(0, nrow = 2, ncol = length(type)))
	colnames(testCrit) <- type
	rownames(testCrit) <- c("R2", "RMSE")
	
    if (!is.null(test)){
		if (dim(test)[2] != dim(data)[2]) {
	        warning("The dimensions of the test set are not correct.")
			test <- NULL
	    }
	}

	argList <- list(...)
	
	id_fmla <- 0; id_deg <- 0; p <- 0; id_gcv <- 0
	for (i in 1:length(type)){
	  type_ <- type[i]
	  if(type_ == "Linear" | type_ == "Additive"){
		if(!is.null(argList$formula)){
			if (class(argList$formula)=="formula"){
				l_fmla <- 1
			} else l_fmla <- length(argList$formula)
			
			if(l_fmla==1){
				fmla <- argList$formula
			} else if(l_fmla>=2 & l_fmla<=length(type)){
				id_fmla <- id_fmla+1
				if (id_fmla > length(type) | id_fmla > l_fmla){
					stop("Argument \'formula'\ has not a valid length.")
				}
				fmla <- argList$formula[[id_fmla]]
			} else if (l_fmla > length(type)){
					stop("Argument \'formula'\ has not a valid length.")
			}
		} else {
			switch(type_, Linear   = { 	warning("argument \'formula\' not found, set at \'Y~.\'")
										fmla <- formulaLm(X,Y)}, 
					  Additive = {  warning("argument \'formula\' not found, set at \'Y~s(X1)+...+s(Xp)\'")
									fmla <- formulaAm(X,Y)})
		}
		modTmp <- modelFit(X, Y, type = type_, formula = fmla)
		} else if (type_ == "StepLinear"){
			if(!is.null(argList$formula)){
				if (class(argList$formula)=="formula"){
					l_fmla <- 1
				} else l_fmla <- length(argList$formula)
			
				if(l_fmla==1){
					fmla <- argList$formula
				} else if(l_fmla>=2 & l_fmla<=length(type)){
					id_fmla <- id_fmla+1
					if (id_fmla > length(type) | id_fmla > l_fmla){
						stop("Argument \'formula'\ has not a valid length.")
					}
					fmla <- argList$formula[[id_fmla]]
				} else if (l_fmla > length(type)){
					stop("Argument \'formula'\ has not a valid length.")
				}
			} else {
				warning("argument \'formula\' not found, set at \'Y~.\'")
				fmla <- formulaLm(X,Y)
			}
			init <- modelFit(X,Y,type="Linear",formula=fmla)
			if (length(init$coefficients)>n) {
				stop("There are too many terms into the full length model")
			}
			if (!is.null(argList$penalty)){
				if(length(argList$penalty)==1){
					penalty <- argList$penalty
				} else if(length(argList$penalty)>=2 & length(argList$penalty)<= length(type)){
					p <- p+1
					penalty <- argList$penalty[p]
				} else if(length(argList$penalty)> length(type)){
					stop("Argument \'penalty'\ has not a valid length.")
				}
			} else {
				warning("argument \'penalty\' not found, set at \'2\' (AIC criteria)")
				penalty <- 2
			}
			modTmp <- modelFit(X, Y, type = type_, formula = fmla,penalty=penalty)
		} else if (type_ == "MARS"){
			if(!is.null(argList$degree)){
				l_deg <- length(argList$degree)
				if(l_deg == 1){ id_deg <- 1}
				else if(l_deg >= 2 & l_deg <= length(type) ){
					id_deg <- id_deg+1
					if (id_deg > length(type) | id_deg > l_deg){
						stop("Argument \'formula'\ has not a valid length.")
					}
				}
				degree <- argList$degree[[id_deg]]
		    } else {
			  warning("argument \'degree\' not found, set at \'2\'")
			  degree <- 2
			}
			modTmp <- modelFit(X, Y, type = type_, degree=degree)
		} else if (type_ == "PolyMARS"){
			if(!is.null(argList$gcv)){
				l_gcv <- length(argList$gcv)
				if(l_gcv == 1){ 
					id_gcv <- 1
				} else if (l_gcv >= 2 & l_gcv <= length(type)){
					id_gcv <- id_gcv+1
					if (id_gcv > length(type) | id_gcv > l_gcv){
						stop("Argument \'gcv'\ has not a valid length.")
					}
				} else if (l_gcv > length(type)){
					stop("Argument \'gcv'\ has not a valid length.")
				}
				gcv <- argList$gcv[[id_gcv]]
			} else {
				warning("argument \'gcv\' not found, set at \'4\'")
				gcv <- 4
			}
			modTmp <- modelFit(X, Y, type = type_, gcv=gcv)
		}
		if(type_ == "Linear" | type_ == "StepLinear" | type_ == "Additive" | type_ == "MARS"){
			learningCrit[1, i] <- R2(Y, modTmp$model$fitted.values)
			learningCrit[2, i] <- RMSE(Y, modTmp$model$fitted.values)
		} else if(type_ == "PolyMARS"){
			learningCrit[1, i] <- R2(Y, modTmp$model$fitted)
			learningCrit[2, i] <- RMSE(Y, modTmp$model$fitted)
		}
		cvTmp <- crossValidation(modTmp,K=10)$Ypred
		CVCrit[1,i] <- R2(Y, cvTmp)
		CVCrit[2,i] <- RMSE(Y, cvTmp)
		if (!is.null(test)){
			Ytest 	<- modelPredict(model = modTmp, newdata = test[, 1:f])
			testCrit[1, i] <- R2(test[,f+1], Ytest)
			testCrit[2, i] <- RMSE(test[,f+1], Ytest)
		}
	}
		
    if (min(testCrit[1, ]) < 0) {
        warning("R2 calculated for the test data is negative.")
    }
    
	if (min(CVCrit[1, ]) < 0) {
        warning("R2 estimated by cross validation (K=10) is negative.")
    }
	
    if (!is.null(test)){
		R2Crit <- data.frame(t(learningCrit[1, ]), t(CVCrit[1,]), t(testCrit[1, ]))
	    colnames(R2Crit) <- c("learning", "cross-validation","test")
	    RMSECrit <- data.frame(t(learningCrit[2, ]), t(CVCrit[2,]), t(testCrit[2, ]))
	    colnames(RMSECrit) <- c("learning", "cross-validation","test")
	} else {
		R2Crit <- data.frame(t(learningCrit[1, ]), t(CVCrit[1,]))
	    colnames(R2Crit) <- c("learning", "cross-validation")
	    RMSECrit <- data.frame(t(learningCrit[2, ]), t(CVCrit[2,]))
	    colnames(RMSECrit) <- c("learning", "cross-validation")
	}
	
    op <- par(ask=TRUE,mfrow = c(1, 2), cex = 0.7)
    dotchart(as.matrix(R2Crit), pch = 19, xlim = c(0, 1),xlab = list("Comparison of R2", cex=1.5, font=2))
	axis(3)
	abline(v = axTicks(1), lty = "dotted", col = "gray60")
    dotchart(as.matrix(RMSECrit), pch = 19, xlab = list("Comparison of RMSE", cex=1.5, font=2), xlim = range(RMSECrit))
	abline(v = axTicks(1), lty = "dotted", col = "gray60")
	axis(3)
    par(op)

	if (is.null(test)){
        return(list(Learning = learningCrit, CV = CVCrit))
    } else return(list(Learning = learningCrit, CV = CVCrit, Test = testCrit))
}