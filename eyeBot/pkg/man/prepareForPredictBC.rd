\name{prepareForPredictBC}
\alias{prepareForPredictBC}

\title{
  Convert node predictions into probabilities for binary classification  models.
}
\description{
This method can only be aplied for a binary classification model. Its primary purpose is to process a \code{\link[randomForest]{randomForest}} object as required for \code{predictBC()}.
This method  converts  node predictions in the \code{\link[randomForest]{randomForest}} object. 
The current class  label in terminal nodes is  replaced by the probability of belonging to a "selected" class - where the probability is calculated as the proportion of local training set instances assigned to the terminal node in question which belong to the "selected" class.
The class of the first instance in the complete training dataset  is chosen as the "selected" class.
} %RMR: Anna - is this correct? Also, is the "selected" class, as explained here, the class assigned the number 1? If so, this should be explained in getChanges.Rd, featureContributions.Rd, getLocalIncrements.rd and predictBC.rd! #<TO DO>: Check this is the case.


\usage{
prepareForPredictBC(object,  dataT, mcls=NULL)
}

\arguments{
 \item{object}{an object of the class \code{randomForest}}
 \item{dataT}{a data frame containing the variables in the model for all instances in the training set} %RMR: Anna, I'm pretty sure this is correct?
 \item{mcls}{main class that be set to "1" for binary classification. If \code{NULL}, the class name from the first record in \code{dataT} will be set as "1"}
}

\value{
 an object of class \code{randomForest} with a new \code{type="binary"}.
}
\author{
Anna Palczewska \email{annawojak@gmail.com}
}

\seealso{
\code{\link[randomForest]{randomForest}}

}
\examples{
\dontrun{
library(randomForest)
data(ames)
ames_train<-ames[ames$Type=="Train",-c(1,3, ncol(ames))]
rF_Model <- randomForest(x=ames_train[,-1],y=as.factor(as.character(ames_train[,1])),
	ntree=500,importance=TRUE, keep.inbag=TRUE,replace=FALSE) 
new_Model<-prepareForPredictBC(rF_Model, ames_train[,-1])
}
}
\keyword{binary}
\keyword{ contribution } 
