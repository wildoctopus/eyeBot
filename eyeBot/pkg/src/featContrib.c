#include "featContrib.h"
//#include <Rmath.h>
//#include <R.h>

/* Node status */
//#define NODE_TERMINAL -1
//#define NODE_TOSPLIT  -2
//#define NODE_INTERIOR -3

//get local increment
void lIncrementReg(double *data, double *out, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, int *inbag, double *localIncrements,
               double *rootMeans){

    memset(localIncrements, 0, *nTrees * *maxNodeNumber * sizeof(double));
    memset(rootMeans, 0, *nTrees * sizeof(double));
    int idx1 = 0;
    int inbag_idx = 0;
    for (int i = 0; i < *nTrees; ++i) 
    {
		tlIncrementReg(data, out, *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, inbag + inbag_idx, localIncrements+idx1, rootMeans+i);
		idx1 += *maxNodeNumber; 
		inbag_idx += *nrRow;
    }
}

#define cathegorical_choice(split, data) (((unsigned int) split) & (((unsigned int) 1) << ((int) (data) - 1)))

void  tlIncrementReg(double *data, double *out, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, int *inbag, double *localIncrements,
               double *rootMeans)
{

	int  j=0,m=0, k=0;
	int inBagCount=0;
	
	int *nodeCount= (int *) Calloc(treeSize, int); 
	memset(nodeCount, 0, treeSize * sizeof(int));
	double *avgPred= (double *) Calloc(treeSize, double); 
	memset(avgPred, 0, treeSize * sizeof(double));
	
	for (j = 0; j < nrRow; ++j) {
		k = 0;
		if(inbag[j]==1)
		{
			inBagCount++;
			while (nodestatus[k] != NODE_TERMINAL) { /* go down the tree */
				m = mbest[k] - 1;
				if (cat[m] == 1) {
					k = (data[m + j*(nrColumn)] <= xsplit[k]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				} else {
					k = cathegorical_choice(xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
			}
			
			nodeCount[k]++;
			avgPred[k] += out[j];
		}	
	}  
  
	meanActivity(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, 0);
	localIncrementsOfContrib(avgPred, nodestatus,lDaughter,rDaughter, NULL, 0, localIncrements);
    *rootMeans=avgPred[0];
	Free (nodeCount);
	Free (avgPred);
}

void meanActivity(double *avgPred,  int *nodeCount, int *nodestatus, int *lDaughter, int *rDaughter, int k)
{ 
	if(nodestatus[k] == NODE_TERMINAL)
	{
       if(nodeCount[k]!= 0) avgPred[k] /= nodeCount[k];
	}
	else
	{
		int lDaugh=lDaughter[k]-1;
		int rDaugh=rDaughter[k]-1;
		
		meanActivity(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, lDaugh);
		meanActivity(avgPred, nodeCount, nodestatus,lDaughter,rDaughter, rDaugh); 
		//Rprintf("node %d, left count  %d, mean %f !\n",k, nodeCount[lDaugh], avgPred[lDaugh]);
		//Rprintf(" node %d, right count  %d, mean %f !\n",k, nodeCount[rDaugh], avgPred[rDaugh]);
		
		nodeCount[k] = nodeCount[lDaugh] + nodeCount[rDaugh];
		if (nodeCount[k] != 0)
			avgPred[k] = (nodeCount[lDaugh]*avgPred[lDaugh] + nodeCount[rDaugh]*avgPred[rDaugh]) / nodeCount[k];
		//Rprintf("node %d, count %d, mean %f !\n",k, nodeCount[k], avgPred[k]);
	}
                   

}

void localIncrementsOfContrib(double *avgPred,  int *nodestatus, int *lDaughter, int *rDaughter, int *p_k, int k, double *localIncrements)
{
	if(p_k)
		localIncrements[k]+=avgPred[k]-avgPred[*p_k];
		
    if(nodestatus[k] != NODE_TERMINAL)
    {
		int lDaugh=lDaughter[k]-1;
		int rDaugh=rDaughter[k]-1;
		
		localIncrementsOfContrib(avgPred, nodestatus,lDaughter,rDaughter, &k, lDaugh, localIncrements);
		localIncrementsOfContrib(avgPred, nodestatus,lDaughter,rDaughter, &k, rDaugh, localIncrements);
		
	}
	
}
//calculate feature contribution	
void fContribReg(double *data, int *nrColumn, int *nrRow,
               int *nTrees, int *lDaughter, int *rDaughter,
               int *nodestatus, int *maxNodeNumber, double *xsplit,
               double *avnodes, int *mbest, int *treeSizes, int *cat, double *localIncrements, double *contrib)
{
  
    memset(contrib, 0, *nrRow  * *nrColumn * sizeof(double));
    int idx1 = 0;
    for (int i = 0; i < *nTrees; ++i) 
    { 
		tfContribReg(data,  *nrColumn, *nrRow, lDaughter+idx1, rDaughter+idx1,nodestatus +idx1,  xsplit+idx1,  mbest+idx1, treeSizes[i], cat, localIncrements+idx1, contrib);       
		idx1 += *maxNodeNumber; /* increment the offset */ 
    }    
    for (int i = (*nrRow) * (*nrColumn) - 1; i >= 0; i--)
		contrib[i] /= *nTrees;				   
}


void tfContribReg(double *data, int nrColumn, int nrRow,
               int *lDaughter, int *rDaughter,
               int *nodestatus, double *xsplit,
               int *mbest, int treeSize, int *cat, double *localIncrements, double *contrib)
{

	int  j=0,m=0, k=0;   
	for (j = 0; j < nrRow; ++j) 
	{
		k = 0;
			while (nodestatus[k] != NODE_TERMINAL) 
			{ // go down the tree 
				int descriptor = mbest[k] - 1;
				if (cat[descriptor] == 1) {
					// Find the child
					k = (data[descriptor + j*(nrColumn)] <= xsplit[k]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}
				else{
				    k = cathegorical_choice (xsplit[k], data[m + j*(nrColumn)]) ? lDaughter[k] - 1 : rDaughter[k] - 1;
				}			
					contrib[descriptor + j*(nrColumn)] += localIncrements[k];	
			}		
	}

}
