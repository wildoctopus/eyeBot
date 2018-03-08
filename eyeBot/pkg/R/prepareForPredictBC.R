"prepareForPredictBC"<-function(object, dataT, mcls=NULL)
{
  if (!inherits(object, "randomForest"))
        stop("Object is not of class randomForest")
  if (is.null(object$forest)) stop("No forest component in the object")

  if (is.null(object$inbag))  stop("No matrix that keeps track of which samples are in-bag in which trees")

  if (object$type != "classification")
        stop("This conversion can be only apply for the clasiffication model")
        
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
    
   if(!is.null(mcls) && !(mcls %in% object$y))  stop("Wrong  give n class in mcls parameter")


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
  
  class1<-NULL
  response<-NULL
  if(object$forest$nclass == 2) 
  {
        if(any(!levels(object$y) %in% c("1","0")))
		{
		
			if(is.null(mcls))
			{
				print(paste("Class ", levels(object$y)[1], " was set to be 1")) 
				class1<-levels(object$y)[1]
			}
			else
			{
				print(paste("Class ", mcls, " was set to be 1")) 
				class1<-mcls
			}
		
		}
            
        if(is.factor(object$y))
        {  
				  if(!is.null(class1))
				  {
					response<-object$y
						if(is.null(mcls)) 
						{
							levels(response)<-c("1","0")
						}
						else{
							levels(response)[levels(response)!= mcls] <- "0"
							levels(response)[levels(response)== mcls] <- "1"
							
						}
					response<-as.numeric(as.character( response))
				   }
				   else{
					response<-as.numeric(as.character( object$y))
					}
        }
        else
        {
               stop("response is not a factor")
        }
        
     
              
        if(!is.null(object$forest$treemap)) {
            
                object$forest$leftDaughter <-
                    object$forest$treemap[,1,, drop=FALSE]
                object$forest$rightDaughter <-
                    object$forest$treemap[,2,, drop=FALSE]
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
           if (! is.double(response))
               storage.mode(response) <- "double"
               
               
       
    #nodepred=object$forest$nodepred
    ans<-.C("votingToProb",
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
     nodePred=as.double(object$forest$nodepred),
     object$forest$bestvar,
     object$forest$ndbigtree,
     object$forest$ncat,
     inbag,
     PACKAGE = "rfFC")
     object$forest$nodepred<- matrix(ans$nodePred, ncol = ntree)[1:nrnodes,, drop=FALSE]
     
      
     }
     else{
     stop("Feature Contribution does not work with multiclassifiers - yet")
     }
    object$type<-"binary"
    object$y.conv<-response
  
    return(object) 
}
