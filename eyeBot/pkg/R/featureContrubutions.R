"featureContributions" <-function(object, lInc, dataT, mClass=NULL)
{
  #test model object
  if (!inherits(object, "randomForest")) stop("featureContributions(): Object is not of class randomForest")
  if (is.null(object$forest)) stop("featureContributions(): No forest component in the object")
  if (is.null(object$inbag))  stop("featureContributions(): No matrix that keeps track of which samples are in-bag in which trees")
  if (object$type == "unsupervised") stop("featureContributions(): Can not calculate descriptor contribution from the unsupervised forest.")
  if (is.na(charmatch(tolower(lInc$type),c("reg", "class")) ))
				stop("featureContributions(): type of localIncrements must be one of 'reg', 'class'")
               
#test training dataset
   if (missing(dataT)) stop("featureContributions(): Training dataset not provided")
   if (inherits(object, "randomForest.formula"))  {
      dataN <- as.data.frame(dataT)
      rn <- row.names(dataN)
      Terms <- delete.response(object$terms)
      x <- model.frame(Terms, dataN, na.action = na.omit) 
   } else
   {
       if (is.null(dim(dataT))) dim(dataT) <- c(1, length(dataT))
       if (nrow(dataT) == 0) stop("featureContributions(): dataT has 0 rows")
        if (any(is.na(dataT)))  stop("featureContributions(): missing values in dataT")
        x<-dataT
   } 
#test number columns. 
	vname <- if (is.null(dim(object$importance)))   names(object$importance)
		else   rownames(object$importance)
    
     if (is.null(colnames(x))) {
        if (ncol(x) != length(vname)) {
            stop("featureContributions(): number of variables in dataT does not match that in the training data")
        }
    } else {
        if (any(! vname %in% colnames(x)))
            stop("featureContributions(): variables in the training data missing in dataT")
        x <- x[, vname, drop=FALSE]
    }
#test categorical data 
	if (is.data.frame(x)) {
		xfactor <- which(sapply(x, is.factor))
	    if (length(xfactor) > 0 && "xlevels" %in% names(object$forest)) {
			for (i in xfactor) {
				if (any(! levels(x[[i]]) %in% object$forest$xlevels[[i]]))
					stop("featureContributions(): New factor levels not present in the training data")
				x[[i]] <-factor(x[[i]], levels=levels(x[[i]])[match(levels(x[[i]]), object$forest$xlevels[[i]])])
			}
		}
		cat.new <- sapply(x, function(x) if (is.factor(x) && !is.ordered(x)) length(levels(x)) else 1)
		if (!all(object$forest$ncat == cat.new))
			stop("featureContributions(): Type of predictors in dataT do not match that of the training data.")
	}

#test local increments  
	if(missing(lInc)) stop(" featureContributions(): localIncrements array not provided")
	if(is.null(lInc$forest)) stop("featureContributions():  localIncrements is null")

 #var
	forest<-object$forest
	inbag<-object$inbag
	mdim <- ncol(x)
	ntest <- nrow(x)
	ntree <- object$forest$ntree
	maxcat <- max(object$forest$ncat)
	nrnodes <- object$forest$nrnodes
    flocalIncrements<-lInc$forest$lIncrements
  

	x.col.names <- if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
	x.row.names <- rownames(x)
    x <- t(data.matrix(x))
    
   if (lInc$type == "reg" )
   {
		fcontrib<-matrix(data=0, nrow=ntest, ncol=mdim)
		if (!is.null(object$forest$treemap)) {
			object$forest$leftDaughter <- object$forest$treemap[,1,, drop=FALSE]
            object$forest$rightDaughter <-object$forest$treemap[,2,, drop=FALSE]
            object$forest$treemap <- NULL
		}
		## Ensure storage mode is what is expected in C.
		if (! is.integer(object$forest$leftDaughter))
			storage.mode(object$forest$leftDaughter) <- "integer"
		if (! is.integer(object$forest$rightDaughter))
			storage.mode(object$forest$rightDaughter) <- "integer"
		if (! is.integer(object$forest$nodestatus))
			storage.mode(object$forest$nodestatus) <- "integer"
		if (! is.double(object$forest$xbestsplit))
			storage.mode(object$forest$xbestsplit) <- "double"
		if (! is.double(object$forest$nodepred))
		   storage.mode(object$forest$nodepred) <- "double"
		if (! is.integer(object$forest$bestvar))
		   storage.mode(object$forest$bestvar) <- "integer"
		if (! is.integer(object$forest$ndbigtree))
		   storage.mode(object$forest$ndbigtree) <- "integer"
		if (! is.integer(object$forest$ncat))
		   storage.mode(object$forest$ncat) <- "integer"
		if (! is.double(flocalIncrements))
		   storage.mode(flocalIncrements) <- "double"

	    ans<-.C("fContribReg",
	     as.double(x),
	     as.integer(mdim),
	     as.integer(ntest),
	     as.integer(ntree),
	     object$forest$leftDaughter,
	     object$forest$rightDaughter,
	     object$forest$nodestatus,
	     nrnodes,
	     object$forest$xbestsplit,
	     object$forest$nodepred,
	     object$forest$bestvar,
	     object$forest$ndbigtree,
	     object$forest$ncat,
	     flocalIncrements,
	     contrib=as.double(fcontrib),
	     PACKAGE = "rfFC")
	   contrib<-matrix(ans$contrib, byrow = TRUE, ncol = mdim, dimnames = list(x.row.names, x.col.names))
	   return(contrib)
     }
     else{
     
        fcontrib<-matrix(data=0, nrow=ntest, ncol=mdim)
#		if(!is.factor(object$y)) #object$y<-as.numeric(object$y)
 #        stop("featureContributions(): response is not a factor")
        
        if(is.null(mClass)) predictedvalue <- predict(object, dataT) 
        else{
			print(paste("Predict towards class", mClass, sep=" " ))
			if (! any (mClass == object$y)) stop("featureContributions(): unknown class id mClass")
			
			predictedvalue <- rep (mClass, times=nrow(dataT))
        } 
        
        if(!is.factor(predictedvalue)) predictedvalue <- factor(predictedvalue, levels=levels(object$y))
           
        nodepred<-as.numeric(predictedvalue) 
             
        if (!is.null(object$forest$treemap)) {      
                object$forest$leftDaughter <-object$forest$treemap[,1,, drop=FALSE]
                object$forest$rightDaughter <-object$forest$treemap[,2,, drop=FALSE]
                object$forest$treemap <- NULL
        }

		## Ensure storage mode is what is expected in C.
		if (! is.integer(object$forest$leftDaughter))
			storage.mode(object$forest$leftDaughter) <- "integer"
		if (! is.integer(object$forest$rightDaughter))
			storage.mode(object$forest$rightDaughter) <- "integer"
		if (! is.integer(object$forest$nodestatus))
			storage.mode(object$forest$nodestatus) <- "integer"
		if (! is.double(object$forest$xbestsplit))
			storage.mode(object$forest$xbestsplit) <- "double"
		if (! is.double(object$forest$nodepred))
		   storage.mode(object$forest$nodepred) <- "double"
		if (! is.integer(object$forest$bestvar))
		   storage.mode(object$forest$bestvar) <- "integer"
		if (! is.integer(object$forest$ndbigtree))
		   storage.mode(object$forest$ndbigtree) <- "integer"
		if (! is.integer(object$forest$ncat))
		   storage.mode(object$forest$ncat) <- "integer"    
	    if (! is.integer(nodepred))
		   storage.mode(nodepred) <- "integer"
		if (! is.double(flocalIncrements))
		   storage.mode(flocalIncrements) <- "double" 

	    ans<-.C("fContribClass",
	     as.double(x),
	     as.integer(mdim),
	     as.integer(ntest),
	     as.integer(ntree),
	     object$forest$leftDaughter,
	     object$forest$rightDaughter,
	     object$forest$nodestatus,
	     nrnodes,
	     object$forest$xbestsplit,
	     object$forest$bestvar,
	     nodepred,
	     object$forest$ndbigtree,
	     object$forest$ncat,
	     flocalIncrements,
	     contrib=as.double(fcontrib),
	     length(object$classes),
	     PACKAGE = "rfFC")  
      contrib<-matrix(ans$contrib, byrow = TRUE, ncol = mdim, dimnames = list(x.row.names, x.col.names))
      return(contrib)    
  }
}
