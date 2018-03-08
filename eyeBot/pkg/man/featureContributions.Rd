\name{featureContributions}
\alias{featureContributions}
\title{
 Feature Contributions for a Random Forest Model
}
\description{
This method calculates  feature contributions for a given dataset and  an existing Random Forest model \code{\link[randomForest]{randomForest}}. The feature contributions 
are computed separately for each instance/record in dataset and provide detailed information about relationships
between variables and the predicted value.    
This method was implemented  based upon the approach of Kuz'min \emph{et al.}  for regression models and  extended to classification models. 
For a binary classification model the method returns the feature contributions towards class "one". For a multi-class model, 
the feature contributions are calculated towards the class predicted by the \code{randomForest} model for a given instance.

The method does not work for unsupervised models. 
The \code{randomForest} model must have a stored in-bag matrix that keeps track of 
 which samples were used  to build trees in the forest and sampling without replacement must be used to generate a model. 
 
Hence, all Random Forest models analyzed by this method must be generated as follows:


           \code{model <- randomForest(...,keep.inbag=TRUE,replace=FALSE) }
 
 
The reason for this current limitation is because, in the code of the \code{randomForest} implementation of Random Forest provided by Liaw and Wiener, the \code{inbag} matrix does not record how many times a sample was used to build a particular tree (if sampling with replacement).
} 

\usage{
featureContributions(object, lInc, dataT, mClass=NULL)
}

\arguments{
  \item{object}{an object of the class \code{randomForest}}
  \item{lInc}{local increments of feature contributions calculated for this object using \cr
              \code{getLocalIncrements}}
  \item{dataT}{a data frame containing the variables in the model (columns) for all instances (rows) for which feature contributions are desired}
  \item{mClass}{a name of the class to which feature contributions is calculated. The class name must to match to the one class name from the randomForest
  object variable \code{y}. By default, the value of this parameter is set to \code{NULL}. 
  In this case the feature contributions are calculated to the predicted class returned by the \code{predict} method from the 
  \code{\link[randomForest]{randomForest}} package. This option is avaliable only for the multiclassification  problems.}
}

\value{
 A list with the  following components:
 \item{contrib}{\code{n x m} matrix of feature contributions, 
 where \code{n} is the number of records (i.e. instances) and \code{m} is the number of features/variables (i.e. attributes/descriptors) of the dataset 
 \code{dataT}.\cr 
 For regression, or multi-class classification, the feature contributions represent the signed contributions towards the predicted value or a 
 given class defined by the argument \code{mClass}. \cr
 For binary classification, by default, the classes are internally treated as numeric 1 and 0 and the feature contributions represent 
 the signed contributions towards "class 1". If the class labels in the training set are presented as "1" and "0", 
 then the corresponding classes will be internally treated as 1 and 0 respectively; otherwise, 
 this mapping will be performed arbitrarily with the class of the first instance in the training set treated internally as "class 1", or 
 to the class   provided as a parameter in \code{\link{getLocalIncrements}}. }
 
}  

\references{
V.E. Kuz'min  et al. (2011), Interpretation of QSAR  Models Based on Random Forest Methods, \emph{Molecular Informatics}, 30, 593-603.\cr 
A. Palczewska et al. (2013), Interpreting random forest models using a feature contribution method, \emph{ Proceedings of the 2013 IEEE 14th 
International Conference on Information Reuse and Integration IEEE IRI 2013}, August 14-16, 2013, San Francisco, California, USA, 112-119. \cr
A. Palczewska et al. (2014), Interpreting random forest classification models using a feature contribution method. \emph{in Integration of Reusable Systems, ser. Advances in Intelligent and Soft Computing}, T. Bouabana-Tebibel and S. H. Rubin, Eds. Springer International Publishing, 263, 193-218. 
}

\author{
Anna Palczewska \email{annawojak@gmail.com}    and\cr 
Richard  Marchese Robinson   \email{rmarcheserobinson@gmail.com}
}

\seealso{
\code{\link[randomForest]{randomForest}}, \code{\link{getLocalIncrements}}

}
\examples{

#Multi-class Classification
library(randomForest)
data(iris)
rF <- randomForest(x=iris[,-5],y=as.factor(as.character(iris[,5])),
    ntree=25,importance=TRUE, keep.inbag=TRUE,replace=FALSE) 
#Get Local feature incremets    
li<-getLocalIncrements(rF, iris[,-5])
#Calculate feature contributions
fc<-featureContributions(rF, li, iris[,-5])    

\dontrun{
#Binary classification
library(randomForest)
data(ames)
ames_train<-ames[ames$Type=="Train",-c(1,3, ncol(ames))]
rF_Model <- randomForest(x=ames_train[,-1],y=as.factor(as.character(ames_train[,1])),
          ntree=500,importance=TRUE, keep.inbag=TRUE,replace=FALSE) 
li <- getLocalIncrements(rF_Model,ames_train[,-1])
fc<-featureContributions(rF_Model, li, ames_train[,-1])
}

%Regression
%RMR:%<TO DO>: ONCE ANNA HAS ADDED THE EXAMPLE REGRESSION DATASET!
}

\keyword{ feature }
\keyword{ contribution }
