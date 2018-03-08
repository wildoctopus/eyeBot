\name{checkForestUnanimity}
\alias{checkForestUnanimity}

\title{Check the unanimity of all trees in the Random Forest model} 
\description{

This method checks the \strong{unanimity} of all individual trees in the forest for classification models: this denotes the condition that, for any given leaf (i.e. terminal) node of the tree,
 all instances in the training set assigned to that node should belong to a single class. If this holds for a single tree, the tree is considered \strong{unanimous}. 
 Only if this condition -i.e. that all trees are \strong{unanimous} -  holds will the predictions obtained (for "class 1") for a binary classification model using predict(...,type="prob") and predictBC(...) be the same. 
}
\usage{
checkForestUnanimity(object, dataT)

}

\arguments{
 \item{object}{an object of the class \code{randomForest}}
 \item{dataT}{a data frame with columns containing the attributes (descriptors) for all instances (rows) in the training set of the \code{randomForest} object} 
}

\value{
A list with the  following components:
\item{dec}{\code{TRUE} if all trees in the forest are \strong{unanimous}, otherwise \code{FALSE} }
\item{tcCount}{a list providing the number of training set instances in each class for each terminal node in all trees. 
Where the number 0 is presented for all classes, the corresponding node is not a terminal node.}
\item{tuStatus}{a vector, with one element per tree, denoting whether or not that tree was \strong{unanimous} (TRUE) or not (FALSE)}
}
\author{
Anna Palczewska \email{annawojak@gmail.com} 
}

\seealso{
\code{\link[randomForest]{randomForest}}

}
\examples{
#Iris dataset
library(randomForest)
data(iris)
rF_Model <- randomForest(x=iris[,-5],y=as.factor(as.character(iris[,5])), 
                              ntree=10,importance=TRUE, keep.inbag=TRUE,replace=FALSE)

#Check unanimity
itest<-checkForestUnanimity(rF_Model, iris[,-5])



\dontrun{
# Ames dataset
data(ames)
ames_train<-ames[ames$Type=="Train",-c(1,3, ncol(ames))]
rF_Model <- randomForest(x=ames_train[,-1],y=as.factor(as.character(ames_train[,1])), 
                                ntree=500,importance=TRUE, keep.inbag=TRUE,replace=FALSE)
itest<-checkForestUnanimity(rF_Model, ames_train[,-1])
}
}
\keyword{binary}
\keyword{ contribution }
