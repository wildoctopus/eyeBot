#include <Rmath.h>
#include <R.h>

/* Node status */
#define NODE_TERMINAL -1
#define NODE_TOSPLIT  -2
#define NODE_INTERIOR -3

#define cathegorical_choice(split, data) (((unsigned int) split) & (((unsigned int) 1) << ((int) (data) - 1)))
//regression
void meanActivity(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k);

void localIncrementsOfContrib(double *avgPred,  int *nodestatus, int *lDaughter, int *rDaughter, int *p_k, int k, double *localIncrements);

void lIncrementReg(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag, double *localIncrements,
               double *rootMeans);
               
void  tlIncrementReg(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *localIncrements,
               double *rootMeans);

void fContribReg(double *data, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, double *localIncrements, double *contrib);
               
void tfContribReg(double *data, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, double *localIncrements, double *contrib);
               
//classification

               
void meanActivityClass(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k, int nrClass);

void localIncrementsOfContribClass(double *avgPred,  int *nodestatus, int *lDaughter, int *rDaughter, int *p_k, int k, 
				double *localIncrements, int nrClass); 
				
void lIncrementClass(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag, double *localIncrements,
               double *rootMeans, int *nrClass);
				
void  tlIncrementClass(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *localIncrements,
               double *rootMeans,  int *nrClass);	
			
void fContribClass(double *data, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit, int *mbest, int *nodepredict, int *treeSizes, int *cat,
               double *localIncrements, double *contrib, int *nrClass);			

void tfContribClass(double *data, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest,  int *nodepred, int treeSize, int *cat, double *localIncrements, double *contrib, int *nrClass);

//prediction         

void votingToProb(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *nodepred, int *mbest, int *treeSizes, int *cat, int *inbag);
               
void  treeVoteToProb(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *nodepred);
               
void calculateProb(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k, double* nodepred);
               
void  treePred(double *data, double *yPred, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *nodepred);
void predict(double *data, double *yPred, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *nodepred, int *mbest, int *treeSizes, int *cat, int *inbag) ;


//unanimity       
               
void forestUnanimity(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag,  int *nclass, double *funamity);
void  treeUnanimity(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag,  int *nrClass, double *funamity);
