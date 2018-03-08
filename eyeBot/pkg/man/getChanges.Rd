\name{getChanges}
\alias{getChanges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get change in prediction for an updated feature. 
}
\description{
 This method calculates the changes in predictions, for a pre-determined Random Forest model and set of instances, 
 resulting from updating the value(s) of a specified (vector of) feature(s). The method works with regression and
 classification models.  In case of binary classification  the predictions are calculated  by \code{predictBC()} or \code{predict.randomForest} 
 and represent the probabilities of being in a given class. If the model was obtained directly via the \code{randomForest()} 
 function, the type of predictions calculated correspond to \code{predict.randomForest()}. 
   However, if the model is a binary classification model and was obtained via post-processing 
   the original model from \code{randomForest()}, using \code{prepareForPredictBC()}, 
   the type of predictions calculated correspond to \code{predictBC()}.
}
\usage{
getChanges(features, dataT, object, value=NULL, type=NULL, mcls=NULL)
}

\arguments{
  \item{features}{a vector of the feature numbers/names  to be updated}
  \item{dataT}{a data frame containing the variables in the model for all instances for which changes in predictions are desired}
  \item{object}{an object of the class \code{randomForest}}
  \item{value}{a vector of new feature values for the features provided in \code{features} N.B. If this is set to NULL, and the specified features are binary, 
  the prediction changes reported are those associated with the only possible change in value for these features: from 1 to 0 or vice-versa.} 
  \item{type}{the type of the predictions considered for classification models, by default it is set to \code{type="prob"} 
  but can be set to \code{type="votes"}. 
   }
   \item{mcls}{main class that be set to "1" for binary classification. If \code{NULL}, the class name from the first record in \code{dataT} 
   will be set as "1"} 

}
\value{
A matrix \code{n x m} of  prediction changes, \code{n} is the number of instances in \code{dataT} and \code{m} is the number of  updated features.   
}

\author{
Anna Palczewska \email{annawojak@gmail.com} and \cr Richard Marchese Robinson \email{rmarcheserobinson@gmail.com}
}

\seealso{
\code{\link[randomForest]{randomForest}}
}
\examples{


library(randomForest)
data(ames)
ames_train<-ames[ames$Type=="Train",-c(1,3, ncol(ames))]
ames_train<-ames_train[1:100,]
rF_Model <- randomForest(x=ames_train[,-1],y=as.factor(as.character(ames_train[,1])),
  ntree=500,importance=TRUE, keep.inbag=TRUE,replace=FALSE) %
gc <- getChanges(c(1,166), ames_train, rF_Model)
change<-getChanges(c(1), ames_train[1, ], rF_Model, value = c(0.49))
}

\keyword{ feature }
\keyword{ contribution }
