"checkForestUnanimity" <-function(object, dataT)
{
#test model object
	if (!inherits(object, "randomForest")) stop("Object is not of class randomForest")
	if (is.null(object$forest)) stop("No forest component in the object")
	if (is.null(object$inbag))  stop("No matrix that keeps track of which samples are in-bag in which trees") 	
	if (object$type == "regression") stop("Can not check the unanimity for the regression model")
	
if (missing(dataT)) stop("Training dataset not provided")
   if (inherits(object, "randomForest.formula"))  {
        dataN <- as.data.frame(dataT)
        rn <- row.names(dataN)
        if(!is.null(object$predicted))
        {
            if(length(rn) != length(object$predicted))  stop("number of samples in dataT not equal to that in the training data")
			if(any(! rn %in% names(object$predicted)))  stop("dataT may be not an original training dataset for the random forest object")
		}
        Terms <- delete.response(object$terms)
        x <- model.frame(Terms, dataN, na.action = na.omit)
       
   } else
   {
       if (is.null(dim(dataT)))
			dim(dataT) <- c(1, length(dataT))
       if (nrow(dataT) == 0)
            stop("dataT has 0 rows")
        if (any(is.na(dataT)))
            stop("missing values in dataT")
        if(!is.null(object$predicted))
        {
            keep <- 1:nrow(dataT)
            rn <- rownames(dataT)
            if (is.null(rn)) rn <- keep
            if (nrow(dataT) != length(object$predicted))  stop("number of samples in dataT not equal to that in the training data")
			if(any(! rn %in% names(object$predicted)))  stop("dataT may be not an original training dataset for the random forest object")
		}    
        #x <- data.matrix(dataT)
        x<-dataT
   } 


#test number columns. 
	vname <- if (is.null(dim(object$importance))) {
        names(object$importance)
    } else {
        rownames(object$importance)
    }
    
     if (is.null(colnames(x))) {
        if (ncol(x) != length(vname)) {
            stop("number of variables in dataT does not match that in the training data")
        }
    } else {
        if (any(! vname %in% colnames(x)))
            stop("variables in the training data missing in dataT")
        x <- x[, vname, drop=FALSE]
    }
#test categorical data

 
  if (is.data.frame(x)) {
        xfactor <- which(sapply(x, is.factor))
        if (length(xfactor) > 0 && "xlevels" %in% names(object$forest)) {
            for (i in xfactor) {
                if (any(! levels(x[[i]]) %in% object$forest$xlevels[[i]]))
                    stop("New factor levels not present in the training data")
                x[[i]] <-
                    factor(x[[i]],
                           levels=levels(x[[i]])[match(levels(x[[i]]), object$forest$xlevels[[i]])])
            }
        }
        cat.new <- sapply(x, function(x) if (is.factor(x) && !is.ordered(x))
                          length(levels(x)) else 1)
        if (!all(object$forest$ncat == cat.new))
            stop("Type of predictors in dataT do not match that of the training data.")
    }

	
	
	forest<-object$forest
	inbag<-object$inbag
	mdim <- ncol(x)
	ntest <- nrow(x)
	ntree <- object$forest$ntree
	maxcat <- max(object$forest$ncat)
	nrnodes <- object$forest$nrnodes
	funanimity<-matrix(data=0, nrow=ntree, ncol=nrnodes*length(object$classes))
	x <- t(data.matrix(x))
	
	if (!is.null(object$forest$treemap)) {
	
		object$forest$leftDaughter <-
		object$forest$treemap[,1,, drop=FALSE]
		object$forest$rightDaughter <-
		object$forest$treemap[,2,, drop=FALSE]
		object$forest$treemap <- NULL
	}
	response<-NULL
  
	if(is.factor(object$y)){
		response<-as.numeric(object$y)
    }
    else stop("response is not a factor")
         
	

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
    
 
	
	ans<-.C("forestUnanimity",
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
	length(object$classes),
	unanimity=as.double(funanimity),
	PACKAGE = "rfFC")
	

    classCount <-tapply(ans$unanimity,rep(1:ntree,each=nrnodes*length(object$classes)),function(i){matrix(i,byrow=T,nrow=nrnodes, ncol=length(object$classes)) })
	treePurity<-unlist(lapply(classCount, function(x){ifelse(any(apply(x,1,max) < apply(x,1,sum)), FALSE,TRUE)}))
	decision<-all(treePurity)
    out<-list(dec=decision, tcCount=classCount, tuStatus=treePurity)   
	return(out)
 
}
