"getLocalIncrements" <-function(object, dataT, binAsReg=TRUE, mcls=NULL)
{
  #test model object
   if (!inherits(object, "randomForest")) stop("getLcalIncrements(): Object is not of class randomForest")
   if (is.null(object$forest)) stop("getLcalIncrements(): No forest component in the object")
   if (is.null(object$inbag))  stop("getLcalIncrements(): No matrix that keeps track of which samples are in-bag in which trees")
   if (object$type == "unsupervised")  stop("getLcalIncrements(): Can not calculate descriptor contribution from the unsupervised forest.")
   if (is.na(charmatch(tolower(object$type),c("regression", "classification"))) )
				stop("getLcalIncrements(): type must be one of 'regression', 'classification'")
				

        
#test training dataset
   if (missing(dataT)) stop("getLcalIncrements(): Training dataset not provided")
   if (inherits(object, "randomForest.formula"))  {
		dataN <- as.data.frame(dataT)
		rn <- row.names(dataN)
		if(!is.null(object$predicted))
		{
			if(length(rn) != length(object$predicted))  stop("getLcalIncrements(): number of samples in dataT not equal to that in the training data")
			if(any(! rn %in% names(object$predicted)))  stop("getLcalIncrements(): dataT may be not an original training dataset for the random forest object")
		}
		Terms <- delete.response(object$terms)
		x <- model.frame(Terms, dataN, na.action = na.omit)
   } else{
       if (is.null(dim(dataT))) dim(dataT) <- c(1, length(dataT))
       if (nrow(dataT) == 0) stop("getLcalIncrements(): dataT has 0 rows")
       if (any(is.na(dataT))) stop("getLcalIncrements(): missing values in dataT")
       if(!is.null(object$predicted)){
            keep <- 1:nrow(dataT)
            rn <- rownames(dataT)
            if (is.null(rn)) rn <- keep
            if (nrow(dataT) != length(object$predicted))  stop("getLcalIncrements(): number of samples in dataT not equal to that in the training data")
			if(any(! rn %in% names(object$predicted)))  stop("getLcalIncrements(): dataT may be not an original training dataset for the random forest object")
		}    
        x<-dataT
   } 

#test features  number and names 
	vname <- if (is.null(dim(object$importance))) {
		names(object$importance)
	} else {
		rownames(object$importance)
	}
    
	if (is.null(colnames(x))) {
		if (ncol(x) != length(vname)) stop("getLcalIncrements(): number of variables in dataT does not match that in the training data")
	} else {
		if (any(! vname %in% colnames(x))) 	stop("getLcalIncrements(): variables in the training data missing in dataT")
		x <- x[, vname, drop=FALSE]
	}

#test categorical data
	if (is.data.frame(x)) {
		xfactor <- which(sapply(x, is.factor))
		if (length(xfactor) > 0 && "xlevels" %in% names(object$forest)) {
			for (i in xfactor) {
				if (any(! levels(x[[i]]) %in% object$forest$xlevels[[i]]))
					stop("getLcalIncrements():  New factor levels not present in the training data")
				x[[i]] <- factor(x[[i]], levels=levels(x[[i]])[match(levels(x[[i]]), object$forest$xlevels[[i]])])
			}
		}
		cat.new <- sapply(x, function(x) if (is.factor(x) && !is.ordered(x))
						  length(levels(x)) else 1)
		if (!all(object$forest$ncat == cat.new)) stop("Type of predictors in dataT do not match that of the training data.")
	}
	
   if(!is.null(mcls) && !(mcls %in% object$y))  stop("Wrong  give n class in mcls parameter")

   response<-NULL 				
   if (binAsReg && object$type == "classification" && object$forest$nclass == 2){
      if(any(!levels(object$y) %in% c("1","0"))){
         response<-object$y
         if(is.null(mcls)){
			print(paste("getLcalIncrements(): Class ", levels(object$y)[1], " was set to be 1")) 
			levels(response)<-c("1","0")
		 }
		 else{
		    print(paste("getLcalIncrements(): Class ", mcls, " was set to be 1")) 
			levels(response)[levels(response)!= mcls] <- "0"
			levels(response)[levels(response)== mcls] <- "1"
		}
		    
			response<-as.numeric(as.character( response))	
					
		}
		else{
		  
		  response<-as.numeric(as.character(object$y))
		
		}
   }
   
   

#vars
	forest<-object$forest
	inbag<-object$inbag
	mdim <- ncol(x)
	ntest <- nrow(x)
	ntree <- object$forest$ntree
	maxcat <- max(object$forest$ncat)
	nrnodes <- object$forest$nrnodes


  
	x.col.names <- if (is.null(colnames(x))) 1:ncol(x) else colnames(x)
	x.row.names <- rownames(x)
    x <- t(data.matrix(x))
  
    if (object$type == "regression"  || !is.null(response)){  
	   
	   flocalIncrements<-matrix(data=0, nrow=ntree, ncol=nrnodes)
	   frootMeans<-matrix(data=0, nrow=1, ncol=ntree)
	   
       if (!is.null(object$forest$treemap)) {
           object$forest$leftDaughter <- object$forest$treemap[,1,, drop=FALSE]
           object$forest$rightDaughter <- object$forest$treemap[,2,, drop=FALSE]
           object$forest$treemap <- NULL
       }
       
	    if(is.null(response)) response<-object$y   
	
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
		if (! is.integer(inbag))
		   storage.mode(inbag) <- "integer"
	   if (! is.double(response))
		   storage.mode(response) <- "double"
	
	    ans<-.C("lIncrementReg",
	     as.double(x),
	     response,
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
	     inbag,
	     localIncrements=as.double(flocalIncrements),
	     rootMeans=as.double(frootMeans),
	     PACKAGE = "rfFC")

	    out<-list(type="reg", forest=list(lIncrements=ans$localIncrements, rmv=ans$rootMeans))
	    return(out)
    }
    else{
	   flocalIncrementsClass<-matrix(data=0, nrow=ntree, ncol=nrnodes*length(object$classes))
	   frootMeans<-matrix(data=0, nrow=length(object$classes), ncol=ntree)
	   
 	   if(is.factor(object$y)) 	  object$y<-as.numeric(object$y)
	   else  stop("getLcalincrements(): response is not a factor")
	   
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
		if (! is.integer(inbag))
		   storage.mode(inbag) <- "integer"
	   if (! is.double(object$y))
		   storage.mode(object$y) <- "double"

		ans<-.C("lIncrementClass",
		 as.double(x),
		 object$y,
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
		 inbag,
		 localIncrements=as.double(flocalIncrementsClass),
		 rootMeans=as.double(frootMeans),
		 length(object$classes), 
		 PACKAGE = "rfFC")
		 
	  
	  out<-list(type="class", forest=list(lIncrements=ans$localIncrements, rmv=ans$rootMeans))
	  return(out)
	}
}

