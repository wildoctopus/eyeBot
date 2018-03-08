\name{ames}
\docType{data}
\alias{ames}
\title{Hansen Ames Mutagenicity Dataset}
\description{This binary classification dataset 
corresponds to the benchmark dataset for Ames mutagenicity modelling presented by Hansen \emph{et al.} }
\usage{data(ames)}
\format{ \code{ames} is a data frame with 6512 cases (rows) and  170
  variables (columns). This data set consists of two types of
  Activity: (1) positive, (0) negative chemical compounds. All chemical 
  structures are encoded  using  binary attributes: a bit vector  calculated
  based upon the MACCS key fingerprint. Dataset structure: CAS_NO, Activity, Canonical_Smiles, 
  X1-X166 - 166 binary descriptors, Type. The CAS_NO column presents the CAS number (i.e. the instance ID). 
  The 'Training' and 'Test' labels in the Type column denote the first of the five splits presented by Hansen \emph{et al.}
}
%\source{
%  Ohis binary classification dataset 
%corresponds to the benchmark dataset for Ames mutagenicity modelling presented by Hansen et al.
%}
\references{
      K. Hansen, S. Mika, T. Schroeter, A. Sutter, A. ter Laak, T. Steger-Hartmann, 
      N. Heinrich, K.-R. Mueller (2009),Benchmark Data Set for in Silico Prediction of Ames Mutagenicity.
       \emph{Journal of Chemical Information and Modeling}, 49, 2077-2081.
}

\keyword{datasets}