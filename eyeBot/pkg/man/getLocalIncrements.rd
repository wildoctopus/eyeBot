\name{getLocalIncrements}
\alias{getLocalIncrements}
\title{
 Get Local Increments of Feature Contributions for a Random Forest Model
}
\description{
 This method calculates  local increments of  feature contributions from  an existing  \code{randomForest} model. This method was implemented  based upon the approach of Kuz'min et al.  for regression models and  extended to classification models.  The method does not work for unsupervised models.  The \code{randomForest} model must have a stored in-bag matrix that keeps track of 
 which samples were used  to build trees in the forest and sampling without replacement must be used to generate a model. 
 Hence, all Random Forest models analyzed by getLocalIncrements() and, subsequently, \code{featureContributions()}, must be generated as follows:
 model <- randomForest(...,keep.inbag=TRUE,replace=FALSE)
 The reason for this current limitation is because, in the code of the \code{randomForest} implementation of Random Forest provided by Liaw and Wiener, 
 the \code{inbag} matrix does not record how many times a sample was used to build a particular tree (if sampling with replacement).
 The method returns  local increments for all nodes in each tree for regression and binary classification models. 
 In case of multi-classification problems the method returns the local increments  calculated for all classes for every tree node in the forest.
 }

\usage{
getLocalIncrements(object,  dataT, binAsReg=TRUE, mcls=NULL)
}

\arguments{
  \item{object}{an object of the class \code{randomForest}}
  \item{dataT}{a data frame containing the variables in the model for all instances for which feature contributions are desired}
  \item{binAsReg}{this option is only relevant for binary classification. If  \code{TRUE} (default), the binary classification model is treated like 
  a regression model,for the purpose of calculating feature contributions, with the class labels treated as numeric values of 1 or 0. If \code{FALSE}, 
  only the local increments in favour of the predicted class (for the forest as a whole) are calculated - as per the treatment of multi-class classifiers.} 
  \item{mcls}{main class that be set to "1" for binary classification. If \code{NULL}, the class name from the first record in \code{dataT} will be set as "1",  
  otherwhise the provided class will be map to "1".}
}

\value{
A list with the  following components:
\item{type}{the type of the method used for calculating local increments of feature contributions} %RMR: Anna, you need to be explicit about what each of the options means! #<TO DO:?: Check this myself.>
\item{forest}{If a multi-class classification model, or a binary classification model analyzed using the binAsReg=FALSE option, has been analyzed, this is a list that contains: a vector \code{lIncrements} of local increments  for all classes and each node of each tree, and a \code{k x ntree} matrix \code{rmv}  of  the mean proportion of instances in each class in the root nodes, where \code{k} is the number of classes and \code{ntree} is the number of trees in the forest.
If a regression model,or a binary classification model analyzed using the binAsReg=TRUE option, has been analyzed, 
 this is this is a list that contains: a vector \code{lIncrements} of local increments  
 for all classes and each node of each tree, and another vector, of length \code{ntree}, \code{rmv}  of  the mean activity (with the two classes treated as numeric values of 1 or 0 in the case of binary classification) of instances in the root nodes.} %RMR: Anna, is this correct? I have only briefly examined this for a binary classification model analyzed using default options - for which it looks correct based on a brief examination. #<TO DO>: Go over this myself.
}

\references{
V.E. Kuz'min  et al. (2011). Interpretation of QSAR  Models Based on Random Forest Methods, \emph{Molecular Informatics}, 30, 593-603. %
\cr
A. Palczewska et al. (2013), Interpreting random forest models using a feature contribution method, \emph{ Proceedings of the 2013 IEEE 14th 
International Conference on Information Reuse and Integration IEEE IRI 2013}, August 14-16, 2013, San Francisco, California, USA, 112-119.\cr
A. Palczewska et al. (2014), Interpreting random forest classification models using a feature contribution method. \emph{in Integration of Reusable Systems, ser. Advances in Intelligent and Soft Computing}, T. Bouabana-Tebibel and S. H. Rubin, Eds. Springer International Publishing, 263, 193-218. 
}
\author{
Anna Palczewska \email{annawojak@gmail.com} and \cr
Richard  Marchese Robinson   \email{rmarcheserobinson@gmail.com}
}



\seealso{
\code{\link[randomForest]{randomForest}}

}
\examples{
 \dontrun{
#Binary classification 
library(randomForest)
data(ames)
ames_train<-ames[ames$Type=="Train",-c(1,3, ncol(ames))]
rF_Model <- randomForest(x=ames_train[,-1],y=as.factor(as.character(ames_train[,1])),
         ntree=500,importance=TRUE, keep.inbag=TRUE,replace=FALSE) 
li <- getLocalIncrements(rF_Model,ames_train[,-1])
}
}

\keyword{ feature }
\keyword{ contribution }% 
