#only one feature chenge
"getChanges"<-function(features, dataT, object, value=NULL, type=NULL, mcls=NULL)
{
   #test model object
  if (!inherits(object, "randomForest")) stop("Object is not of class randomForest")
  if(missing(features)) stop("no features provided to test changes")
  if(missing(dataT)) stop("no dataset provided to test changes")
  if(object$type=="classification" || object$type=="binary") classRF<-TRUE
  else classRF<-FALSE
  
  #if(classRF && length(levels(object$y))>2) stop("the number of classes is greater then 2 - the method works only with the binary classifer")
  
  if(!is.null(type))
  {
       pred.type <- charmatch(tolower(type),c("prob", "vote"))
       if (is.na(pred.type)) stop("getChanges(): type must be one of 'prob', 'vote'")
       type.prop=type
  }
  #test if the features  are in dataT
  
  if (inherits(object, "randomForest.formula"))  {
        dataN <- as.data.frame(dataT)
        rn <- row.names(dataN)
        #if(!is.null(object$predicted))
        #{
            #if(length(rn) != length(object$predicted))  stop("number of samples in dataT not equal to that in the training data")
			#if(any(! rn %in% names(object$predicted)))  stop("dataT may be not an original training dataset for the random forest object")
		#}
        Terms <- delete.response(object$terms)
        x <- model.frame(Terms, dataN, na.action = na.omit)
        
       
   } else
   {
       if (is.null(dim(dataT)))
			dim(dataT) <- c(1, length(dataT))
       if (nrow(dataT) == 0)
            stop("getChanges(): dataT has 0 rows")
        if (any(is.na(dataT)))
            stop("getChanges(): missing values in dataT")
        if(!is.null(object$predicted))
        {
            keep <- 1:nrow(dataT)
            rn <- rownames(dataT)
            if (is.null(rn)) rn <- keep
		}    
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
            stop("getChanges(): number of variables in dataT does not match to that in the model training data")
        }
    } else {
        if (any(! vname %in% colnames(x)))
            stop("getChanges(): variables in the training data missing in dataT")
        x <- x[, vname, drop=FALSE]
    }
    

#test categorical data
   if(is.character(features))
   {
       if (any(! features %in% colnames(x)))
            stop("getChanges(): variables in the training data missing in dataT")
       featureNames<-features
      
   }
   else if(is.numeric(features))
   {
      if(any (features < 1)) stop("getChanges(): wrong feature number")
      featureNames<-colnames(x)[features]      
      
   }
   else{
     stop("getChanges(): wrong format of feature number/names vector")
   }

  featureDescription<-rep(0, length(features))
  if(!is.null(value))
  {
    if(length(features)!= length(value)) stop("getChanges(): value vector has different size then features vector")
    #if(!is.numeric(value))stop("value vector must be numerical")
  
  }
  #else{
  #  print("This version works only with binary description")
  #TODO: ADD categorical features
  #}
  if(!is.null(mcls) && !(mcls %in% object$y))  stop("getChanges(): Wrong  given class name  in mcls parameter")
  
  class1<-NULL
  if(any(!levels(object$y) %in% c("1","0")) && classRF )
  {
  
       if(is.null(mcls))
       {
			print(paste(" getChanges(): Class ", levels(object$y)[1], " was set to be 1")) 
			class1<-levels(object$y)[1]
		}
		else
		{
			print(paste("getChanges(): Class ", mcls, " was set to be 1")) 
			class1<-mcls
		}
		
	
  }
  
  
 
#TODO add prediction for multiclassification
#add checking the categorical variable 
  
    
  if(object$type== "binary") originPred<-predictBC(object, dataT)
  else if(object$type == "regression") originPred<-predict(object, dataT)
  else if(object$type == "classification")
  {
	if(is.null(type)) originPred<-predict(object, dataT, type="prob")
	else originPred<-predict(object, dataT, type=type.prop)
	if(is.matrix(originPred)) 
	{
		if(!is.null(class1)) originPred<-originPred[,class1]
		else  originPred<-originPred[,"1"]
		
    }
  }
  else stop("getChanges(): Wrong randomForest model type")
  

 #to do check types of the features and values
  if(is.factor(originPred))
  originPred<-as.numeric(as.character(originPred))
  change<-originPred
  for(i in 1:length(featureNames))
  {
		
		convertedData<-x;
		#check if  feature numeric or binary
		for(j in 1:nrow(convertedData))
		{
		     if(is.null(value))
		     {
				convertedData[j, featureNames[i]]<-if(convertedData[j, featureNames[i]]==1) 0 else 1
		     }
		     else
		     {
		 
		       if(is.numeric(dataT[,featureNames[i]])) convertedData[j, featureNames[i]]<- convertedData[j, featureNames[i]] + as.numeric(value[i])
		       else if(is.factor(dataT[,featureNames[i]]))	{ 
					if(!(value[i] %in%  levels(convertedData[,featureNames[i]]))) stop (paste("Wrong category for the  column ", featureNames[i]))
					else   	convertedData[j,featureNames[i]]<-value[i]
					}
		       else convertedData[j, featureNames[i]]<- value[i]
		     }
		}

		
		  if(object$type== "binary")  new_pred<-predictBC(object, convertedData)
		  else if(object$type == "regression") 		new_pred<-predict(object, convertedData)
		  else if(object$type == "classification")
		  {
				if(is.null(type))
				{
					new_pred<-predict(object, convertedData, type="prob")
				}
				else {
					new_pred<-predict(object, convertedData, type=type.prop)
				}
				if(is.matrix(new_pred)) 
				{
					if(!is.null(class1)) new_pred<-new_pred[,class1]
					else  new_pred<-new_pred[,"1"]
				}
			}
			else
			{
				stop("getChanges(): Wrong randomForest model type")
			}
		
		
	
		if(is.factor(new_pred)) ##check if numeric first
			new_pred<-as.numeric(as.character(new_pred))
		
		if(i==1){
			change<-new_pred-originPred
		}
        else
		{		
		  change<-cbind(change, (new_pred-originPred))
		
		}
				 
  }
  change<-as.matrix(change)
  rownames(change)<-rownames(dataT)
  colnames(change)<-featureNames
  return(change) 
   
   
}
